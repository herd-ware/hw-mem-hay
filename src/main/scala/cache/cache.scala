/*
 * File: cache.scala                                                           *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-02 01:39:51 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay.cache

import chisel3._
import chisel3.util._

import herd.common.field._
import herd.common.mem.cbo.{OP, SORT, BLOCK}
import herd.mem.hay.common._


class Cache(p: CacheParams) extends Module {
  require(isPow2(p.nSet), "Cache must have 2^N sets.")
  require(isPow2(p.nMem), "Cache must have 2^N memories.")
  require(p.nMem <= p.nSet, "Sets must be more or as numerous as memories.")

  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(0, p.nDataBit))) else None
    val b_part = if (p.useField) Some(new NRsrcIO(1, p.nField, p.nPart)) else None
    
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(1, p.useField, p.nField, p.nTagBit, p.nSet)))

    val b_acc = Flipped(Vec(p.nAccess, new CacheAccIO(p)))    
    val b_rep = Flipped(new CacheRepIO(p))
    
    val b_read = Flipped(Vec(p.nReadPort, new CacheReadIO(p)))
    val b_write = Flipped(Vec(p.nWritePort, new CacheWriteIO(p)))

    val i_pend = Input(Vec(p.nPendingAcc, new CachePendBus(p)))
    val i_end = Input(new CachePendBus(p))
  }) 

  val m_ctrl = Seq.fill(p.nSet){Module(new Ctrl(p))}
  val m_mem = Seq.fill(p.nMem){Module(new Mem(p.pMem))}

  // ******************************
  //            ACCESS
  // ******************************
  // Default
  for (s <- 0 until p.nSet) {
    if (p.useField) {
      m_ctrl(s).io.b_field.get <> io.b_field.get
      m_ctrl(s).io.b_part.get <> io.b_part.get
    }
    
    m_ctrl(s).io.b_cbo <> io.b_cbo
    
    m_ctrl(s).io.b_acc <> io.b_acc
    for (a <- 0 until p.nAccess) {
      m_ctrl(s).io.b_acc(a).valid := false.B
      io.b_acc(a).ready := false.B
    }
  }

  // Connect
  for (a <- 0 until p.nAccess) {
    for (s <- 0 until p.nSet) {
      when(s.U === io.b_acc(a).set) {
        m_ctrl(s).io.b_acc(a) <> io.b_acc(a)
      }
    }
  }

  // ******************************
  //            REPLACE
  // ******************************
  // Default
  for (s <- 0 until p.nSet) {
    m_ctrl(s).io.b_rep <> io.b_rep
    m_ctrl(s).io.b_rep.valid := false.B
    io.b_rep.ready := false.B    
  }
  
  // Connect
  for (s <- 0 until p.nSet) {
    when(s.U === io.b_rep.set) {
      m_ctrl(s).io.b_rep <> io.b_rep
    }
  }

  // ******************************
  //             READ
  // ******************************
  val r_read_mem = Reg(Vec(p.nReadPort, UInt(log2Ceil(p.nMem).W)))

  // Default
  for (r <- 0 until p.nReadPort) {
    for (m <- 0 until p.nMem) {
      m_mem(m).io.b_read(r) := DontCare
      m_mem(m).io.b_read(r).valid := false.B
    }
    io.b_read(r).ready := false.B
    io.b_read(r).rdata := DontCare
  }    

  // Connect
  for (r <- 0 until p.nReadPort) {
    for (m <- 0 until p.nMem) {
      when (m.U === io.b_read(r).mem) {
        m_mem(m).io.b_read(r).valid := io.b_read(r).valid
        m_mem(m).io.b_read(r).mask := io.b_read(r).mask
        m_mem(m).io.b_read(r).offset := io.b_read(r).offset
        m_mem(m).io.b_read(r).addr := io.b_read(r).data
        io.b_read(r).ready := m_mem(m).io.b_read(r).ready
      }
    }
  }

  // Connect response (1 cycle later)
  for (r <- 0 until p.nReadPort) {
    r_read_mem(r) := io.b_read(r).mem
    for (m <- 0 until p.nMem) {
      when (m.U === r_read_mem(r)) {
       io.b_read(r).rdata := m_mem(m).io.b_read(r).rdata
      }
    }
  }

  // ******************************
  //             WRITE
  // ******************************
  // Default
  for (w <- 0 until p.nWritePort) {
    for (m <- 0 until p.nMem) {
      m_mem(m).io.b_write(w) := DontCare
      m_mem(m).io.b_write(w).valid := false.B
    }
    io.b_write(w).ready := false.B
  } 

  // Connect
  for (w <- 0 until p.nWritePort) {
    for (m <- 0 until p.nMem) {
      when (m.U === io.b_write(w).mem) {
        m_mem(m).io.b_write(w).valid := io.b_write(w).valid
        m_mem(m).io.b_write(w).mask := io.b_write(w).mask
        m_mem(m).io.b_write(w).offset := io.b_write(w).offset
        m_mem(m).io.b_write(w).addr := io.b_write(w).data
        m_mem(m).io.b_write(w).wdata := io.b_write(w).wdata
        io.b_write(w).ready := m_mem(m).io.b_write(w).ready
      }
    }
  }

  // ******************************
  //            PENDING
  // ******************************
  for (s <- 0 until p.nSet) {
    m_ctrl(s).io.i_pend := io.i_pend
    for (pa <- 0 until p.nPendingAcc) {
      m_ctrl(s).io.i_pend(pa).valid := io.i_pend(pa).valid & (s.U === io.i_pend(pa).set)
    }
  }

  // ******************************
  //              END
  // ******************************
  for (s <- 0 until p.nSet) {
    m_ctrl(s).io.i_end := io.i_end
    m_ctrl(s).io.i_end.valid := io.i_end.valid & (s.U === io.i_end.set)    
  }

  // ******************************
  //             CBO
  // ******************************
  for (c <- 0 until p.nCbo) {
    val w_ready = Wire(Vec(p.nSet, Bool()))    

    for (s <- 0 until p.nSet) {
      w_ready(s) := m_ctrl(s).io.b_cbo(c).ready

      m_ctrl(s).io.b_cbo(c).valid := io.b_cbo(c).valid & ((io.b_cbo(c).block === BLOCK.FULL) | (s.U === io.b_cbo(c).set))
      io.b_cbo(c).ready := ((io.b_cbo(c).block === BLOCK.FULL) & w_ready.asUInt.andR) | ((io.b_cbo(c).block =/= BLOCK.FULL) & w_ready(io.b_cbo(c).set))
    }
  }

  // ******************************
  //            FIELD
  // ******************************
  if (p.useField) {
    for (pa <- 0 until p.nPart) {
      val w_free = Wire(Vec(p.nSet, Bool()))

      for (s <- 0 until p.nSet) {
        w_free(s) := m_ctrl(s).io.b_part.get.state(pa).free
      }

      io.b_part.get.state(pa).free := w_free.asUInt.andR
    }

    for (f <- 0 until p.nField) {
      io.b_field.get(f).free := true.B
    }
  } 

  // ******************************
  //             DEBUG
  // ******************************
  if (p.debug) {
    
  }
}

object Cache extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Cache(CacheConfigBase), args)
}