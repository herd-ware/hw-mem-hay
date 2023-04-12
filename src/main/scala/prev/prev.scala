/*
 * File: prev.scala                                                            *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-04-11 05:54:44 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay.prev

import chisel3._
import chisel3.util._

import herd.common.gen._
import herd.common.field._
import herd.common.tools._
import herd.common.core.{HpcCacheBus}
import herd.common.mem.mb4s._
import herd.mem.hay.common._
import herd.mem.hay.cache._
import herd.mem.hay.next._
import herd.mem.hay.pftch.{PftchAccIO, PftchReadIO, PftchWriteIO}


class Prev (p: PrevParams) extends Module {
  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useField, p.nField, p.nTagBit, p.nSet)))

    val i_slct = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_port = MixedVec(
      for (prev <- p.pPrevBus) yield {
        Flipped(new Mb4sIO(prev))
      }
    )

    val b_cache_acc   = Vec(p.nPrevPort, new CacheAccIO(p))
    val b_cache_rep   = Vec(p.nFieldSlct, new CacheRepIO(p))
    val b_cache_read  = Vec(p.nPrevPort, new CacheReadIO(p))
    val b_cache_write = if (!p.readOnly) Some(Vec(p.nPrevPort, new CacheWriteIO(p))) else None

    val b_pftch_acc   = if (p.usePftch) Some(Vec(p.nPrevPort, new PftchAccIO(p, p.nPftchEntry))) else None
    val b_pftch_read  = if (p.usePftch) Some(Vec(p.nPrevPort, new PftchReadIO(p, p.nPftchEntry))) else None
    val b_pftch_write = if (p.usePftch && !p.readOnly) Some(Vec(p.nPrevPort, new PftchWriteIO(p, p.nPftchEntry))) else None

    val b_next_ctrl = Vec(p.nPrevPort, new NextCtrlIO(p, p.nHart))
    val b_next_data = if (!p.readOnly) Some(Vec(p.nPrevPort, new GenDRVIO(p, UInt(0.W), UInt((p.nDataByte * 8).W)))) else None

    val o_miss = Output(Vec(p.nPrevPort, new MissBus(p, p.nHart)))
    val o_hpc = Output(Vec(p.nHart, new HpcCacheBus()))
    val o_pend = Output(Vec(p.nPendingAcc, new CachePendBus(p)))
  })

  // ******************************
  //            MODULES
  // ******************************
  val m_unit = for (unit <- p.pPrev) yield {
    val m_unit = Module(new PrevUnit(unit))
    m_unit
  } 
  val m_rsv = if (p.useAmo) Some(Module(new Reserve(p))) else None
  val m_zero = if (!p.readOnly) Some(Module(new Zero(p))) else None

  // ******************************
  //            CONNECT
  // ******************************
  for (pp <- 0 until p.nPrevPort) {
    if (p.useField) m_unit(pp).io.b_field.get <> io.b_field.get
    m_unit(pp).io.b_cbo <> io.b_cbo

    // ------------------------------
    //             PORT
    // ------------------------------
    if (p.pPrev(pp).useFieldSlct) m_unit(pp).io.i_slct.get := io.i_slct.get  
    m_unit(pp).io.b_port <> io.b_port(pp)
    if (!p.readOnly) {
      if (!p.pPrev(pp).readOnly) {
        m_unit(pp).io.b_zero.get <> m_zero.get.io.b_port(pp)
      } else {
        for (f <- 0 until p.nFieldTag) {
          m_zero.get.io.b_port(pp)(f).ready := false.B
        }
      }
    }

    // ------------------------------
    //             CACHE
    // ------------------------------
    m_unit(pp).io.b_cache_acc <> io.b_cache_acc(pp)
    m_unit(pp).io.b_cache_read <> io.b_cache_read(pp)
    if (!p.readOnly) {
      if (!p.pPrevBus(pp).readOnly) {
        m_unit(pp).io.b_cache_write.get <> io.b_cache_write.get(pp)
      } else {
        io.b_cache_write.get(pp) := DontCare
        io.b_cache_write.get(pp).valid := false.B
      }      
    }

    // ------------------------------
    //           PREFETCHER
    // ------------------------------
    if (p.usePftch) {
      m_unit(pp).io.b_pftch_acc.get <> io.b_pftch_acc.get(pp)
      m_unit(pp).io.b_pftch_read.get <> io.b_pftch_read.get(pp)
      if (!p.readOnly) {
        if (!p.pPrevBus(pp).readOnly) {
          m_unit(pp).io.b_pftch_write.get <> io.b_pftch_write.get(pp)
        } else {
          io.b_pftch_write.get(pp) := DontCare
          io.b_pftch_write.get(pp).valid := false.B
        }      
      }
    }
    
    // ------------------------------
    //         NEXT CONTROLLER
    // ------------------------------
    m_unit(pp).io.b_next_ctrl <> io.b_next_ctrl(pp)
    if (!p.readOnly) {
      if (!p.pPrevBus(pp).readOnly) {
        if (p.useFieldSlct == p.pPrevBus(pp).useFieldSlct) {
          m_unit(pp).io.b_next_data.get <> io.b_next_data.get(pp)
        } else {
          m_unit(pp).io.b_next_data.get.ready(0) := false.B
          for (fs <- 0 until p.nFieldSlct) {
            io.b_next_data.get(pp).valid(fs) := m_unit(pp).io.b_next_data.get.valid(0) & (fs.U === m_unit(pp).io.b_next_data.get.field.get)
            io.b_next_data.get(pp).data.get(fs) := m_unit(pp).io.b_next_data.get.data.get(0)
            when (fs.U === m_unit(pp).io.b_next_data.get.field.get) {
              m_unit(pp).io.b_next_data.get.ready(0) := io.b_next_data.get(pp).ready(fs)
            }
          }
        }        
      } else {
        io.b_next_data.get(pp) := DontCare
        for (fs <- 0 until p.nFieldSlct) {
          io.b_next_data.get(pp).valid(fs) := false.B
        }        
      }      
    }

    // ------------------------------
    //             INFOS
    // ------------------------------
    io.o_miss(pp) := m_unit(pp).io.o_miss
    var pa: Int = 0
    for (pp <- 0 until p.nPrevPort) {
      for (ppa <- 0 until p.pPrev(pp).nPendingAcc) {
        io.o_pend(pa + ppa) := m_unit(pp).io.o_pend(ppa)
      }
      pa = pa + p.pPrev(pp).nPendingAcc
    }
    
    io.o_hpc := 0.U.asTypeOf(io.o_hpc) 
    for (h <- 0 until p.nHart) {
      for (pp <- 0 until p.nPrevPort) {
        io.o_hpc(h).hit(pp) := m_unit(pp).io.o_hpc(h).hit(0)
        io.o_hpc(h).pftch(pp) := m_unit(pp).io.o_hpc(h).pftch(0)
        io.o_hpc(h).miss(pp) := m_unit(pp).io.o_hpc(h).miss(0)
      }
    }
  }

  // ******************************
  //          CACHE REPLACE
  // ******************************
  val w_cache_rep_valid = Wire(Vec(p.nFieldSlct, Vec(p.nPrevPort, Bool())))
  val w_cache_rep_av = Wire(Vec(p.nFieldSlct, Vec(p.nPrevPort, Bool())))

  // ------------------------------
  //            DEFAULT
  // ------------------------------
  for (fs <- 0 until p.nFieldSlct) {
    io.b_cache_rep(fs) := DontCare
    io.b_cache_rep(fs).valid := false.B
  }

  for (pp <- 0 until p.nPrevPort) {
    m_unit(pp).io.b_cache_rep := DontCare
    m_unit(pp).io.b_cache_rep.ready := false.B

    if (p.useFieldSlct) {
      for (fs <- 0 until p.nFieldSlct) {
        w_cache_rep_valid(fs)(pp) := m_unit(pp).io.b_cache_rep.valid & (m_unit(0).io.b_cache_rep.field.get === fs.U)
      }
    } else {
      w_cache_rep_valid(0)(pp) := m_unit(pp).io.b_cache_rep.valid
    }
  }

  // ------------------------------
  //            SELECT
  // ------------------------------
  for (fs <- 0 until p.nFieldSlct) {
    w_cache_rep_av(fs)(0) := ~w_cache_rep_valid(fs)(0)
    when (w_cache_rep_valid(fs)(0)) {
      m_unit(0).io.b_cache_rep <> io.b_cache_rep(fs)
    }

    for (pp <- 1 until p.nPrevPort) {
      w_cache_rep_av(fs)(pp) := w_cache_rep_av(fs)(pp - 1) & ~w_cache_rep_valid(fs)(pp)
      when (w_cache_rep_av(fs)(pp - 1) & w_cache_rep_valid(fs)(pp)) {
        m_unit(pp).io.b_cache_rep <> io.b_cache_rep(fs)
      }
    }
  }

  // ******************************
  //            DEPEND
  // ******************************
  val m_dep_req = Module(new ReqDepend(p))
  val m_dep_acc = if (p.useAccReg) Some(Module(new AccDepend(p))) else None
  val m_dep_read = Module(new ReadDepend(p))

  var ndep: Int = 0
  for (pp <- 0 until p.nPrevPort) {
    for (fs <- 0 until p.pPrevBus(pp).nFieldSlct) {
      // ------------------------------
      //            REQ STAGE
      // ------------------------------
      m_dep_req.io.b_back(ndep + fs) <> m_unit(pp).io.b_dep_back(fs)
      m_dep_req.io.b_bus(ndep + fs) <> m_unit(pp).io.b_dep_bus(fs)
      if (p.useReqReg) m_dep_req.io.b_reg.get(ndep + fs) <> m_unit(pp).io.b_dep_req.get(fs)
      if (p.useAccReg) {
        m_dep_req.io.i_acc(ndep + fs) := m_unit(pp).io.b_dep_acc.get(fs).reg
      } else {
        m_dep_req.io.i_acc(ndep + fs) := m_unit(pp).io.b_dep_read(fs).reg
      }

      // ------------------------------
      //            ACC STAGE
      // ------------------------------
      if (p.useAccReg) {
        m_dep_acc.get.io.b_acc(ndep + fs) <> m_unit(pp).io.b_dep_acc.get(fs)
        m_dep_acc.get.io.i_read(ndep + fs) := m_unit(pp).io.b_dep_read(fs).reg
      }

      // ------------------------------
      //           READ STAGE
      // ------------------------------
      if (p.useAccReg) {
        m_dep_read.io.i_acc(ndep + fs) := m_unit(pp).io.b_dep_acc.get(fs).reg
      } else if (p.useReqReg) {
        m_dep_read.io.i_acc(ndep + fs) := m_unit(pp).io.b_dep_req.get(fs).reg
      } else {
        m_dep_read.io.i_acc(ndep + fs) := m_unit(pp).io.b_dep_bus(fs).reg
      }
      m_unit(pp).io.i_dep_rw(fs) := m_dep_read.io.o_dep_rw(ndep + fs)
      m_dep_read.io.b_read(ndep + fs) <> m_unit(pp).io.b_dep_read(fs)
      if (p.useAckReg && !p.readOnly) m_dep_read.io.i_ack.get(ndep + fs) := m_unit(pp).io.o_dep_ack.get(fs)
    }

    ndep = ndep + p.pPrevBus(pp).nFieldSlct
  }  

  // ******************************
  //            RESERVE
  // ******************************
  if (p.useAmo) {
    for (pp <- 0 until p.nPrevPort) {
      if (p.pPrev(pp).useAmo) {
        m_rsv.get.io.b_port(pp) <> m_unit(pp).io.b_rsv.get
      } else {
        m_rsv.get.io.b_port(pp) := DontCare
        m_rsv.get.io.b_port(pp).valid := false.B
      }
    }
  }

  // ******************************
  //             CBO
  // ******************************
  if (!p.readOnly) m_zero.get.io.b_cbo <> io.b_cbo
  for (c <- 0 until p.nCbo) {
    val w_ready = Wire(Vec(p.nPrevPort, Bool()))

    for (pp <- 0 until p.nPrevPort) {
      w_ready(pp) := m_unit(pp).io.b_cbo(c).ready
    }

    if (!p.readOnly) {
      io.b_cbo(c).ready := (~io.b_cbo(c).zero & w_ready.asUInt.andR) | (io.b_cbo(c).zero & m_zero.get.io.b_cbo(c).ready)
    } else {
      io.b_cbo(c).ready := w_ready.asUInt.andR
    }    
  }

  // ******************************
  //            FIELD
  // ******************************
  if (p.useField) {
    for (f <- 0 until p.nField) {
      val w_free = Wire(Vec(p.nPrevPort, Bool()))

      for (pp <- 0 until p.nPrevPort) {
        w_free(pp) := m_unit(pp).io.b_field.get(f).free
      }

      io.b_field.get(f).free := w_free.asUInt.andR
    }
  }
}

object Prev extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Prev(PrevConfigBase), args)
}