/*
 * File: hay.scala
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-03 02:32:29 pm
 * Modified By: Mathieu Escouteloup
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay

import chisel3._
import chisel3.util._

import herd.common.field._
import herd.common.tools._
import herd.common.core.{HpcCacheBus}
import herd.common.mem.mb4s._
import herd.common.mem.cbo.{CboIO, OP, SORT, BLOCK}
import herd.mem.hay.cache._
import herd.mem.hay.prev._
import herd.mem.hay.next._
import herd.mem.hay.pftch._


class Hay(p: HayParams) extends Module {
  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(p.nAddrBit, p.nDataBit))) else None
    val b_part = if (p.useField) Some(new NRsrcIO(1, p.nField, p.nPart)) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useField, p.nField, p.nAddrBit)))

    val i_slct_prev = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val o_slct_prev = if (p.useFieldSlct) Some(Output(new SlctBus(p.nField, p.nPart, 1))) else None
    val o_slct_next = if (p.useFieldSlct) Some(Output(new SlctBus(p.nField, p.nPart, 1))) else None

    val b_prev = MixedVec(
      for (prev <- p.pPrevBus) yield {
        Flipped(new Mb4sIO(prev))
      }
    )
    val b_next = new Mb4sIO(p.pNextBus)

    val o_hpc = Output(Vec(p.nHart, new HpcCacheBus()))
  })
  
  // ******************************
  //            MODULES
  // ******************************
  val m_cache = Module(new Cache(p))
  val m_prev = Module(new Prev(p))
  val m_next = Module(new Next(p))
  val m_mux = if (p.usePftch) Some(Module(new Mb4sCrossbar(p.pMux))) else None
  val m_pftch = if (p.usePftch) Some(Module(new Pftch(p))) else None

  // ******************************
  //        FIELD INTERFACE
  // ******************************
  val r_slct_req = Reg(new SlctBus(p.nField, p.nPart, 1))
  val r_slct_acc = Reg(new SlctBus(p.nField, p.nPart, 1))
  val r_slct_read = Reg(new SlctBus(p.nField, p.nPart, 1))

  val w_slct_req = Wire(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_acc = Wire(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_read = Wire(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_write = Wire(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_next = Wire(new SlctBus(p.nField, p.nPart, 1))

  if (p.useFieldSlct) {
    w_slct_req := io.i_slct_prev.get
  } else {
    w_slct_req.field := 0.U
    w_slct_req.next := 0.U
    w_slct_req.step := 0.U
  }

  // Prev ReqStage register ?
  r_slct_req := w_slct_req
  if (p.useReqReg) w_slct_acc := r_slct_req else w_slct_acc := w_slct_req

  // Prev AccStage register ?
  r_slct_acc := w_slct_acc
  if (p.useAccReg) w_slct_read := r_slct_acc else w_slct_read := w_slct_acc  
  w_slct_next := r_slct_acc

  // Prev ReadStage register
  r_slct_read := w_slct_read
  w_slct_write := r_slct_read 

  if (p.useFieldSlct) io.o_slct_prev.get := w_slct_write

  // Next memory level
  if (p.useFieldSlct) io.o_slct_next.get := w_slct_next

  // ******************************
  //             PREV
  // ******************************
  if (p.useField) m_prev.io.b_field.get <> io.b_field.get
  for (c <- 0 until p.nCbo) {
    m_prev.io.b_cbo(c).fromGlob(io.b_cbo(c), p.nAddrBit)
  }

  if (p.useFieldSlct) m_prev.io.i_slct.get := io.i_slct_prev.get
  m_prev.io.b_port <> io.b_prev

  // ******************************
  //             NEXT
  // ******************************
  if (p.useField) m_next.io.b_field.get <> io.b_field.get
  for (c <- 0 until p.nCbo) {
    m_next.io.b_cbo(c).fromGlob(io.b_cbo(c), p.nAddrBit)
  }

  if (p.useFieldSlct) {
    m_next.io.i_slct_prev.get := w_slct_acc
    m_next.io.i_slct_write.get := w_slct_write
  }
  m_next.io.b_prev_ctrl <> m_prev.io.b_next_ctrl
  if (!p.readOnly) m_next.io.b_prev_data.get <> m_prev.io.b_next_data.get 

  // ******************************
  //             PFTCH
  // ******************************
  // Prefetcher: connect
  if (p.usePftch) {
    if (p.useField) {
      m_pftch.get.io.b_field.get <> io.b_field.get
      m_pftch.get.io.b_part.get <> io.b_part.get
      m_mux.get.io.b_field.get <> io.b_field.get
    }
    for (c <- 0 until p.nCbo) {
      m_pftch.get.io.b_cbo(c).fromGlob(io.b_cbo(c), p.nAddrBit)
    }

    // Synchronization with other controllers
    if (p.useFieldSlct) {
      m_pftch.get.io.i_slct_acc.get := w_slct_acc
      m_pftch.get.io.i_slct_req.get := w_slct_next
      m_pftch.get.io.i_slct_read.get := w_slct_read
      m_pftch.get.io.i_slct_write.get := w_slct_write
    }

    for (pp <- 0 until p.nPrevPort) {
      m_pftch.get.io.b_prev_acc(pp) <> m_prev.io.b_pftch_acc.get(pp)

      m_pftch.get.io.b_prev_read(pp) <> m_prev.io.b_pftch_read.get(pp)
      if (p.useFieldSlct & !p.pPrevBus(pp).useFieldSlct) {
        m_pftch.get.io.b_prev_read(pp).valid := m_prev.io.b_pftch_read.get(pp).valid & (m_prev.io.b_pftch_read.get(pp).field.get === w_slct_read.field)
        m_prev.io.b_pftch_read.get(pp).ready := m_pftch.get.io.b_prev_read(pp).ready & (m_prev.io.b_pftch_read.get(pp).field.get === w_slct_read.field)
      }

      if (!p.readOnly) {
        m_pftch.get.io.b_prev_write.get(pp) <> m_prev.io.b_pftch_write.get(pp)
        if (p.useFieldSlct & !p.pPrevBus(pp).useFieldSlct) {
          m_pftch.get.io.b_prev_write.get(pp).valid := m_prev.io.b_pftch_write.get(pp).valid & (m_prev.io.b_pftch_write.get(pp).field.get === w_slct_write.field)
          m_prev.io.b_pftch_write.get(pp).ready := m_pftch.get.io.b_prev_write.get(pp).ready & (m_prev.io.b_pftch_write.get(pp).field.get === w_slct_write.field)
        }
      }
    }

    m_pftch.get.io.i_miss := m_prev.io.o_miss
  } 

  // ******************************
  //           NEXT PORT
  // ******************************
  if (p.usePftch) {
    if (p.useField) m_mux.get.io.b_field.get <> io.b_field.get

    if (p.useFieldSlct) {
      m_mux.get.io.i_slct_req.get := w_slct_next
      m_mux.get.io.i_slct_write.get := w_slct_write
      m_mux.get.io.i_slct_read.get := w_slct_read
    }
    m_next.io.b_port <> m_mux.get.io.b_m(0)
    m_pftch.get.io.b_port <> m_mux.get.io.b_m(1)
    m_mux.get.io.b_s(0) <> io.b_next
  } else {
    m_next.io.b_port <> io.b_next
  }

  // ******************************
  //             CACHE
  // ******************************
  if (p.useField) {
    m_cache.io.b_field.get <> io.b_field.get      
    m_cache.io.b_part.get <> io.b_part.get
  }
  for (c <- 0 until p.nCbo) {
    m_cache.io.b_cbo(c).fromGlob(io.b_cbo(c), p.nAddrBit)
  }

  // Accesses and read
  for (pp <- 0 until p.nPrevPort) {
    m_cache.io.b_acc(pp) <> m_prev.io.b_cache_acc(pp)

    m_cache.io.b_read(pp) <> m_prev.io.b_cache_read(pp)
    if (p.useFieldSlct & !p.pPrevBus(pp).useFieldSlct) {
      m_cache.io.b_read(pp).valid := m_prev.io.b_cache_read(pp).valid & (m_prev.io.b_cache_read(pp).field.get === w_slct_read.field)
      m_prev.io.b_cache_read(pp).ready := m_cache.io.b_read(pp).ready & (m_prev.io.b_cache_read(pp).field.get === w_slct_read.field)
    }
  }

  // Write: replace
  m_cache.io.b_write(0) <> m_next.io.b_rep

  // Write: modify
  if (!p.readOnly) {
    for (pp <- 0 until p.nPrevPort) {
      m_cache.io.b_write(pp + 1) <> m_prev.io.b_cache_write.get(pp)
      if (p.useFieldSlct & !p.pPrevBus(pp).useFieldSlct) {
        m_cache.io.b_write(pp + 1).valid := m_prev.io.b_cache_write.get(pp).valid & (m_prev.io.b_cache_write.get(pp).field.get === w_slct_write.field)
        m_prev.io.b_cache_write.get(pp).ready := m_cache.io.b_write(pp + 1).ready & (m_prev.io.b_cache_write.get(pp).field.get === w_slct_write.field)
      }
    }
  }  

  // Write: prefetch
  if (p.usePftch) {
    if (!p.readOnly) {
      m_cache.io.b_write(p.nPrevPort + 1) <> m_pftch.get.io.b_write
    } else {
      m_cache.io.b_write(1) <> m_pftch.get.io.b_write
    }    
  }

  // Replace: default
  if (p.usePftch) {
    m_cache.io.b_rep <> m_pftch.get.io.b_rep
  } else {
    m_cache.io.b_rep := DontCare
    m_cache.io.b_rep.valid := false.B
  }
  
  for (fs <- 0 until p.nFieldSlct) {
    m_prev.io.b_cache_rep(fs) := DontCare
    m_prev.io.b_cache_rep(fs).ready := false.B
  }

  // Replace: select
  for (fs <- 0 until p.nFieldSlct) {
    when (fs.U === w_slct_acc.field) {
      if (p.usePftch) {
        when (m_prev.io.b_cache_rep(fs).valid) {
          m_cache.io.b_rep <> m_prev.io.b_cache_rep(fs)
          m_pftch.get.io.b_rep.ready := false.B
        }        
      } else {
        m_cache.io.b_rep <> m_prev.io.b_cache_rep(fs)
      }      
    }
  }

  // Pending accesses
  m_cache.io.i_pend := m_prev.io.o_pend

  // End
  if (p.usePftch) {
    when (m_next.io.o_end.valid) {
      m_cache.io.i_end := m_next.io.o_end
    }.otherwise {
      m_cache.io.i_end := m_pftch.get.io.o_end
    }    
  } else {
    m_cache.io.i_end := m_next.io.o_end
  }

  // ******************************
  //             HPC
  // ******************************
  io.o_hpc := m_prev.io.o_hpc

  // ******************************
  //            FLUSH
  // ******************************
  for (c <- 0 until p.nCbo) {
    if (p.usePftch) {
      m_pftch.get.io.b_cbo(c).valid := io.b_cbo(c).valid & m_prev.io.b_cbo(c).ready & m_next.io.b_cbo(c).ready
      m_cache.io.b_cbo(c).valid := io.b_cbo(c).valid & m_prev.io.b_cbo(c).ready & m_next.io.b_cbo(c).ready & m_pftch.get.io.b_cbo(c).ready

      io.b_cbo(c).ready := m_prev.io.b_cbo(c).ready & m_next.io.b_cbo(c).ready & m_cache.io.b_cbo(c).ready & m_pftch.get.io.b_cbo(c).ready
    } else {
      m_cache.io.b_cbo(c).valid := io.b_cbo(c).valid & m_prev.io.b_cbo(c).ready & m_next.io.b_cbo(c).ready

      io.b_cbo(c).ready := m_prev.io.b_cbo(c).ready & m_next.io.b_cbo(c).ready & m_cache.io.b_cbo(c).ready
    }    
  }

  // ******************************
  //            FIELD
  // ******************************
  if (p.useField) {
    // ------------------------------
    //          FIELD STATE
    // ------------------------------
    for (f <- 0 until p.nField) {
      if (p.usePftch) {
        m_pftch.get.io.b_field.get(f).flush := io.b_field.get(f).flush & m_prev.io.b_field.get(f).free & m_next.io.b_field.get(f).free
        m_cache.io.b_field.get(f).flush := io.b_field.get(f).flush & m_prev.io.b_field.get(f).free & m_next.io.b_field.get(f).free & m_pftch.get.io.b_field.get(f).free

        io.b_field.get(f).free := m_prev.io.b_field.get(f).free & m_next.io.b_field.get(f).free & m_cache.io.b_field.get(f).free & m_pftch.get.io.b_field.get(f).free
      } else {
        m_cache.io.b_field.get(f).flush := io.b_field.get(f).flush & m_prev.io.b_field.get(f).free & m_next.io.b_field.get(f).free
        io.b_field.get(f).free := m_prev.io.b_field.get(f).free & m_next.io.b_field.get(f).free & m_cache.io.b_field.get(f).free
      }      
    }
    // ------------------------------
    //           PART STATE
    // ------------------------------
    val w_field_flush = Wire(Vec(p.nPart, Bool()))
    val w_ctrl_free = Wire(Vec(p.nPart, Bool()))

    // Global connect
    for (pa <- 0 until p.nPart) {
      w_field_flush(pa) := io.b_field.get(io.b_part.get.state(pa).field).flush

      if (p.usePftch) {
        w_ctrl_free(pa) := m_prev.io.b_field.get(io.b_part.get.state(pa).field).free & m_next.io.b_field.get(io.b_part.get.state(pa).field).free & m_pftch.get.io.b_field.get(io.b_part.get.state(pa).field).free
      } else {
        w_ctrl_free(pa) := m_prev.io.b_field.get(io.b_part.get.state(pa).field).free & m_next.io.b_field.get(io.b_part.get.state(pa).field).free  
      } 

      // Real active flush
      m_cache.io.b_part.get.state(pa).flush := io.b_part.get.state(pa).flush & (~w_field_flush(pa) | w_ctrl_free(pa)) 
      if (p.usePftch) m_pftch.get.io.b_part.get.state(pa).flush := io.b_part.get.state(pa).flush & (~w_field_flush(pa) | w_ctrl_free(pa))

      // Free
      if (p.usePftch) {
        io.b_part.get.state(pa).free := m_cache.io.b_part.get.state(pa).free & m_pftch.get.io.b_part.get.state(pa).free
      } else {
        io.b_part.get.state(pa).free := m_cache.io.b_part.get.state(pa).free
      }
    }         
  }

  // ******************************
  //            DEBUG
  // ******************************
  if (p.debug) {
    dontTouch(m_cache.io.b_acc)
    dontTouch(m_cache.io.b_rep)
    dontTouch(m_cache.io.b_read)
    dontTouch(m_cache.io.b_write)
  }
}

object Hay extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Hay(HayConfigBase), args)
}