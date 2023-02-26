/*
 * File: hay.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:41:21 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay

import chisel3._
import chisel3.util._

import herd.common.dome._
import herd.common.tools._
import herd.common.mem.mb4s._
import herd.common.mem.cbo.{CboIO, OP, SORT, BLOCK}
import herd.mem.hay.cache._
import herd.mem.hay.prev._
import herd.mem.hay.next._
import herd.mem.hay.pftch._


class Hay(p: HayParams) extends Module {
  val io = IO(new Bundle {
    val b_dome = if (p.useDome) Some(Vec(p.nDome, new DomeIO(p.nAddrBit, p.nDataBit))) else None
    val b_part = if (p.useDome) Some(new NRsrcIO(1, p.nDome, p.nPart)) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useDome, p.nDome, p.nAddrBit)))

    val i_slct_prev = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val o_slct_prev = if (p.useDomeSlct) Some(Output(new SlctBus(p.nDome, p.nPart, 1))) else None
    val o_slct_next = if (p.useDomeSlct) Some(Output(new SlctBus(p.nDome, p.nPart, 1))) else None

    val b_prev = MixedVec(
      for (prev <- p.pPrevBus) yield {
        Flipped(new Mb4sIO(prev))
      }
    )
    val b_next = new Mb4sIO(p.pNextBus)

    val o_miss = Output(Vec(p.nHart, UInt(32.W)))
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
  //         DOME INTERFACE
  // ******************************
  val r_slct_req = Reg(new SlctBus(p.nDome, p.nPart, 1))
  val r_slct_acc = Reg(new SlctBus(p.nDome, p.nPart, 1))
  val r_slct_read = Reg(new SlctBus(p.nDome, p.nPart, 1))

  val w_slct_req = Wire(new SlctBus(p.nDome, p.nPart, 1))
  val w_slct_acc = Wire(new SlctBus(p.nDome, p.nPart, 1))
  val w_slct_read = Wire(new SlctBus(p.nDome, p.nPart, 1))
  val w_slct_write = Wire(new SlctBus(p.nDome, p.nPart, 1))
  val w_slct_next = Wire(new SlctBus(p.nDome, p.nPart, 1))

  if (p.useDomeSlct) {
    w_slct_req := io.i_slct_prev.get
  } else {
    w_slct_req.dome := 0.U
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

  if (p.useDomeSlct) io.o_slct_prev.get := w_slct_write

  // Next memory level
  if (p.useDomeSlct) io.o_slct_next.get := w_slct_next

  // ******************************
  //             PREV
  // ******************************
  if (p.useDome) m_prev.io.b_dome.get <> io.b_dome.get
  for (c <- 0 until p.nCbo) {
    m_prev.io.b_cbo(c).fromGlob(io.b_cbo(c), p.nAddrBit)
  }

  if (p.useDomeSlct) m_prev.io.i_slct.get := io.i_slct_prev.get
  m_prev.io.b_port <> io.b_prev

  // ******************************
  //             NEXT
  // ******************************
  if (p.useDome) m_next.io.b_dome.get <> io.b_dome.get
  for (c <- 0 until p.nCbo) {
    m_next.io.b_cbo(c).fromGlob(io.b_cbo(c), p.nAddrBit)
  }

  if (p.useDomeSlct) {
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
    if (p.useDome) {
      m_pftch.get.io.b_dome.get <> io.b_dome.get
      m_pftch.get.io.b_part.get <> io.b_part.get
      m_mux.get.io.b_dome.get <> io.b_dome.get
    }
    for (c <- 0 until p.nCbo) {
      m_pftch.get.io.b_cbo(c).fromGlob(io.b_cbo(c), p.nAddrBit)
    }

    // Synchronization with other controllers
    if (p.useDomeSlct) {
      m_pftch.get.io.i_slct_acc.get := w_slct_acc
      m_pftch.get.io.i_slct_req.get := w_slct_next
      m_pftch.get.io.i_slct_read.get := w_slct_read
      m_pftch.get.io.i_slct_write.get := w_slct_write
    }

    for (pp <- 0 until p.nPrevPort) {
      m_pftch.get.io.b_prev_acc(pp) <> m_prev.io.b_pftch_acc.get(pp)

      m_pftch.get.io.b_prev_read(pp) <> m_prev.io.b_pftch_read.get(pp)
      if (p.useDomeSlct & !p.pPrevBus(pp).useDomeSlct) {
        m_pftch.get.io.b_prev_read(pp).valid := m_prev.io.b_pftch_read.get(pp).valid & (m_prev.io.b_pftch_read.get(pp).dome.get === w_slct_read.dome)
        m_prev.io.b_pftch_read.get(pp).ready := m_pftch.get.io.b_prev_read(pp).ready & (m_prev.io.b_pftch_read.get(pp).dome.get === w_slct_read.dome)
      }

      if (!p.readOnly) {
        m_pftch.get.io.b_prev_write.get(pp) <> m_prev.io.b_pftch_write.get(pp)
        if (p.useDomeSlct & !p.pPrevBus(pp).useDomeSlct) {
          m_pftch.get.io.b_prev_write.get(pp).valid := m_prev.io.b_pftch_write.get(pp).valid & (m_prev.io.b_pftch_write.get(pp).dome.get === w_slct_write.dome)
          m_prev.io.b_pftch_write.get(pp).ready := m_pftch.get.io.b_prev_write.get(pp).ready & (m_prev.io.b_pftch_write.get(pp).dome.get === w_slct_write.dome)
        }
      }
    }

    m_pftch.get.io.i_miss := m_prev.io.o_miss
  } 

  // ******************************
  //           NEXT PORT
  // ******************************
  if (p.usePftch) {
    if (p.useDome) m_mux.get.io.b_dome.get <> io.b_dome.get

    if (p.useDomeSlct) {
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
  if (p.useDome) {
    m_cache.io.b_dome.get <> io.b_dome.get      
    m_cache.io.b_part.get <> io.b_part.get
  }
  for (c <- 0 until p.nCbo) {
    m_cache.io.b_cbo(c).fromGlob(io.b_cbo(c), p.nAddrBit)
  }

  // Accesses and read
  for (pp <- 0 until p.nPrevPort) {
    m_cache.io.b_acc(pp) <> m_prev.io.b_cache_acc(pp)

    m_cache.io.b_read(pp) <> m_prev.io.b_cache_read(pp)
    if (p.useDomeSlct & !p.pPrevBus(pp).useDomeSlct) {
      m_cache.io.b_read(pp).valid := m_prev.io.b_cache_read(pp).valid & (m_prev.io.b_cache_read(pp).dome.get === w_slct_read.dome)
      m_prev.io.b_cache_read(pp).ready := m_cache.io.b_read(pp).ready & (m_prev.io.b_cache_read(pp).dome.get === w_slct_read.dome)
    }
  }

  // Write: replace
  m_cache.io.b_write(0) <> m_next.io.b_rep

  // Write: modify
  if (!p.readOnly) {
    for (pp <- 0 until p.nPrevPort) {
      m_cache.io.b_write(pp + 1) <> m_prev.io.b_cache_write.get(pp)
      if (p.useDomeSlct & !p.pPrevBus(pp).useDomeSlct) {
        m_cache.io.b_write(pp + 1).valid := m_prev.io.b_cache_write.get(pp).valid & (m_prev.io.b_cache_write.get(pp).dome.get === w_slct_write.dome)
        m_prev.io.b_cache_write.get(pp).ready := m_cache.io.b_write(pp + 1).ready & (m_prev.io.b_cache_write.get(pp).dome.get === w_slct_write.dome)
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
  
  for (ds <- 0 until p.nDomeSlct) {
    m_prev.io.b_cache_rep(ds) := DontCare
    m_prev.io.b_cache_rep(ds).ready := false.B
  }

  // Replace: select
  for (ds <- 0 until p.nDomeSlct) {
    when (ds.U === w_slct_acc.dome) {
      if (p.usePftch) {
        when (m_prev.io.b_cache_rep(ds).valid) {
          m_cache.io.b_rep <> m_prev.io.b_cache_rep(ds)
          m_pftch.get.io.b_rep.ready := false.B
        }        
      } else {
        m_cache.io.b_rep <> m_prev.io.b_cache_rep(ds)
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
  //              MISS
  // ******************************
  val w_miss = Wire(Vec(p.nHart, Vec(p.nPrevPort, Bool())))

  for (h <- 0 until p.nHart) {
    for (pp <- 0 until p.nPrevPort) {
      w_miss(h)(pp) := m_prev.io.o_miss(pp).valid & (h.U === m_prev.io.o_miss(pp).hart)
    }
    io.o_miss(h) := PopCount(w_miss(h).asUInt)
  }

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
  //             DOME
  // ******************************
  if (p.useDome) {
    // ------------------------------
    //           DOME STATE
    // ------------------------------
    for (d <- 0 until p.nDome) {
      if (p.usePftch) {
        m_pftch.get.io.b_dome.get(d).flush := io.b_dome.get(d).flush & m_prev.io.b_dome.get(d).free & m_next.io.b_dome.get(d).free
        m_cache.io.b_dome.get(d).flush := io.b_dome.get(d).flush & m_prev.io.b_dome.get(d).free & m_next.io.b_dome.get(d).free & m_pftch.get.io.b_dome.get(d).free

        io.b_dome.get(d).free := m_prev.io.b_dome.get(d).free & m_next.io.b_dome.get(d).free & m_cache.io.b_dome.get(d).free & m_pftch.get.io.b_dome.get(d).free
      } else {
        m_cache.io.b_dome.get(d).flush := io.b_dome.get(d).flush & m_prev.io.b_dome.get(d).free & m_next.io.b_dome.get(d).free
        io.b_dome.get(d).free := m_prev.io.b_dome.get(d).free & m_next.io.b_dome.get(d).free & m_cache.io.b_dome.get(d).free
      }      
    }
    // ------------------------------
    //           PART STATE
    // ------------------------------
    val w_dome_flush = Wire(Vec(p.nPart, Bool()))
    val w_ctrl_free = Wire(Vec(p.nPart, Bool()))

    // Global connect
    for (pa <- 0 until p.nPart) {
      w_dome_flush(pa) := io.b_dome.get(io.b_part.get.state(pa).dome).flush

      if (p.usePftch) {
        w_ctrl_free(pa) := m_prev.io.b_dome.get(io.b_part.get.state(pa).dome).free & m_next.io.b_dome.get(io.b_part.get.state(pa).dome).free & m_pftch.get.io.b_dome.get(io.b_part.get.state(pa).dome).free
      } else {
        w_ctrl_free(pa) := m_prev.io.b_dome.get(io.b_part.get.state(pa).dome).free & m_next.io.b_dome.get(io.b_part.get.state(pa).dome).free  
      } 

      // Real active flush
      m_cache.io.b_part.get.state(pa).flush := io.b_part.get.state(pa).flush & (~w_dome_flush(pa) | w_ctrl_free(pa)) 
      if (p.usePftch) m_pftch.get.io.b_part.get.state(pa).flush := io.b_part.get.state(pa).flush & (~w_dome_flush(pa) | w_ctrl_free(pa))

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