/*
 * File: read.scala                                                            *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-02 01:45:24 pm                                       *
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
import herd.mem.hay.common._
import herd.mem.hay.cache.{CacheReadIO, CachePendBus}
import herd.mem.hay.pftch.{PftchReadIO}


class ReadStage(p: PrevUnitParams) extends Module {
  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useField, p.nField, p.nTagBit, p.nSet)))

    val i_slct = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_in =  Flipped(new GenSRVIO(p, new PrevUnitCtrlBus(p), UInt(0.W)))

    val i_dep_rw = Input(Vec(p.nFieldSlct, Bool()))
    val b_dep = Vec(p.nFieldSlct, new DependStageIO(p))
    val o_pend = Output(Vec(p.nFieldSlct, new CachePendBus(p)))

    val b_read = new CacheReadIO(p)
    val b_pftch = if (p.usePftch) Some(new PftchReadIO(p, p.nPftchEntry)) else None
    
    val o_slct = if (p.useFieldSlct) Some(Output(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_out = new GenSRVIO(p, new PrevUnitCtrlBus(p), Vec(p.nDataByte, UInt(8.W)))
  })

  val m_reg = Module(new GenSReg(p, new PrevUnitCtrlBus(p), UInt(0.W), false, false, true))

  // ******************************
  //            STATUS
  // ******************************
  val w_wait_read = Wire(Bool())
  val w_wait_rw = Wire(Bool())
  val w_wait_reg = Wire(Bool())  

  // ******************************
  //        FIELD INTERFACE
  // ******************************
  val r_slct_out = Reg(new SlctBus(p.nField, p.nPart, 1))

  val w_slct_in = Wire(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_out = Wire(new SlctBus(p.nField, p.nPart, 1))

  if (p.useFieldSlct) {    
    w_slct_in := io.i_slct.get
    r_slct_out := w_slct_in
    w_slct_out := r_slct_out
    io.o_slct.get := w_slct_out
  } else {
    w_slct_in.field := 0.U
    w_slct_in.next := 0.U
    w_slct_in.step := 0.U
    w_slct_out := w_slct_in   
  }

  // ******************************
  //             READ
  // ******************************
  val w_in_pftch = Wire(Bool())
  val w_wait_cache = Wire(Bool())
  val w_wait_pftch = Wire(Bool())

  if (p.usePftch) {
    w_in_pftch := io.b_in.ctrl.get.pftch.get.use
    w_wait_pftch := w_in_pftch & ~io.b_pftch.get.ready  
  } else {
    w_in_pftch := false.B
    w_wait_pftch := false.B  
  }    
  w_wait_cache := ~w_in_pftch & ~io.b_read.ready  

  w_wait_read := io.b_in.valid & io.b_in.ctrl.get.op.ra & (w_wait_cache | w_wait_pftch)

  // ------------------------------
  //             CACHE
  // ------------------------------ 
  io.b_read.valid := io.b_in.valid & io.b_in.ctrl.get.op.ra & ~w_in_pftch & ~w_wait_reg & ~w_wait_rw
  if (p.useField) io.b_read.field.get := io.b_in.field.get
  io.b_read.mask := io.b_in.ctrl.get.op.mask
  io.b_read.mem := io.b_in.ctrl.get.addr.mem()
  io.b_read.data := io.b_in.ctrl.get.addr.memdata()
  io.b_read.offset := io.b_in.ctrl.get.addr.offset

  // ------------------------------
  //            PREFETCH
  // ------------------------------  
  if (p.usePftch) {
    io.b_pftch.get.valid := io.b_in.valid & io.b_in.ctrl.get.op.ra & w_in_pftch & ~w_wait_reg & ~w_wait_rw
    if (p.useField) io.b_pftch.get.field.get := io.b_in.field.get
    io.b_pftch.get.mask := io.b_in.ctrl.get.op.mask
    io.b_pftch.get.entry := io.b_in.ctrl.get.pftch.get.entry
    io.b_pftch.get.data := io.b_in.ctrl.get.addr.data
    io.b_pftch.get.offset := io.b_in.ctrl.get.addr.offset
  }

  // ******************************
  //            OUTPUTS
  // ******************************
  // ------------------------------
  //           REGISTERS
  // ------------------------------
  w_wait_reg := ~m_reg.io.b_sin.ready

  io.b_in.ready := ~(w_wait_reg | w_wait_read | w_wait_rw)

  for (fs <- 0 until p.nFieldSlct) {
    m_reg.io.i_flush(fs) := false.B
  }

  if (p.useFieldSlct) {
    m_reg.io.i_slct_in.get := w_slct_in
    m_reg.io.i_slct_out.get := w_slct_out
  }

  m_reg.io.b_sin.valid := io.b_in.valid & ~w_wait_read & ~w_wait_rw
  if (p.useField) m_reg.io.b_sin.field.get := io.b_in.field.get
  m_reg.io.b_sin.ctrl.get := io.b_in.ctrl.get

  // ------------------------------
  //          READ DATA
  // ------------------------------
  val w_rdata_in = Wire(Vec(p.nDataByte, UInt(8.W)))
  val r_rdata_av = RegInit(VecInit(Seq.fill(p.nFieldSlct)(false.B)))
  val r_rdata = Reg(Vec(p.nFieldSlct, Vec(p.nDataByte, UInt(8.W))))

  if (p.usePftch) {
    w_rdata_in := Mux(m_reg.io.b_sout.ctrl.get.pftch.get.use, io.b_pftch.get.rdata, io.b_read.rdata)
  } else {
    w_rdata_in := io.b_read.rdata
  }  

  when (~r_rdata_av(w_slct_out.field)) {
    r_rdata_av(w_slct_out.field) := m_reg.io.b_sout.valid & ~io.b_out.ready
    r_rdata(w_slct_out.field) := w_rdata_in  
  }.otherwise {
    r_rdata_av(w_slct_out.field) := ~io.b_out.ready
  }

  // ------------------------------
  //            OUTPUTS
  // ------------------------------
  if (p.useField) io.b_out.field.get := m_reg.io.b_sout.field.get
  io.b_out.ctrl.get := m_reg.io.b_sout.ctrl.get
  io.b_out.data.get := Mux(r_rdata_av(w_slct_out.field), r_rdata(w_slct_out.field), w_rdata_in)

  m_reg.io.b_sout.ready := false.B
  io.b_out.valid := false.B
  for (fs <- 0 until p.nFieldSlct) {
    when (fs.U === w_slct_out.field) {
      m_reg.io.b_sout.ready := io.b_out.ready & ~io.b_dep(fs).lock
      io.b_out.valid := m_reg.io.b_sout.valid & ~io.b_dep(fs).lock
    }
  }

  // ******************************
  //            DEPEND
  // ******************************
  w_wait_rw := false.B
  for (fs <- 0 until p.nFieldSlct) {
    when (fs.U === w_slct_in.field) {
      w_wait_rw := io.i_dep_rw(fs)
    }
  }

  for (fs <- 0 until p.nFieldSlct) {
    io.b_dep(fs).reg.valid := m_reg.io.o_val.valid(fs)
    io.b_dep(fs).reg.lock := (fs.U =/= w_slct_out.field) | ~io.b_out.ready
    io.b_dep(fs).reg.hart := m_reg.io.o_val.ctrl.get(fs).info.hart
    io.b_dep(fs).reg.rw := m_reg.io.o_val.ctrl.get(fs).op.wa
    io.b_dep(fs).reg.addr := m_reg.io.o_val.ctrl.get(fs).addr
    if (p.usePftch) io.b_dep(fs).reg.pftch.get := m_reg.io.o_val.ctrl.get(fs).pftch.get      
    if (p.useFieldSlct) {
      io.b_dep(fs).reg.field.get := fs.U
    } else if (p.useField) {
      io.b_dep(0).reg.field.get := m_reg.io.o_val.field.get
    }    
  } 

  // ******************************
  //             PEND
  // ******************************
  for (fs <- 0 until p.nFieldSlct) {
    if (p.usePftch) {
      io.o_pend(fs).valid := m_reg.io.o_val.valid(fs) & m_reg.io.o_val.ctrl.get(fs).op.wa & ~m_reg.io.o_val.ctrl.get(fs).pftch.get.use
    } else {
      io.o_pend(fs).valid := m_reg.io.o_val.valid(fs) & m_reg.io.o_val.ctrl.get(fs).op.wa
    }    
    io.o_pend(fs).line := m_reg.io.o_val.ctrl.get(fs).addr.line
    io.o_pend(fs).set := m_reg.io.o_val.ctrl.get(fs).addr.set
  }  

  // ******************************
  //            CBO
  // ******************************
  for (c <- 0 until p.nCbo) {
    io.b_cbo(c).ready := io.b_cbo(c).valid

    when (io.b_cbo(c).valid & io.b_cbo(c).inv) {
      if (p.useFieldSlct) {
        for (f <- 0 until p.nField) {
          when (f.U === io.b_cbo(c).field.get) {
            io.b_cbo(c).ready := ~m_reg.io.o_val.valid(f)
          }
        }
      } else if (p.useFieldTag) {
        when (io.b_cbo(c).field.get === m_reg.io.o_val.field.get) {
          io.b_cbo(c).ready := ~m_reg.io.o_val.valid(0)
        }
      } else {
        io.b_cbo(c).ready := ~m_reg.io.o_val.valid(0)
      }
    }    
  }

  // ******************************
  //            FIELD
  // ******************************
  if (p.useFieldSlct) {
    for (f <- 0 until p.nField) {
      io.b_field.get(f).free := ~m_reg.io.o_val.valid(f)
    }
  } else if (p.useFieldTag) {
    for (f <- 0 until p.nField) {
      io.b_field.get(f).free := (f.U =/= m_reg.io.o_val.field.get) | ~m_reg.io.o_val.valid(0)
    }
  }   
  
  // ******************************
  //             DEBUG
  // ******************************
  if (p.debug) {
    // ------------------------------
    //            SIGNALS
    // ------------------------------

    // ------------------------------
    //         DATA FOOTPRINT
    // ------------------------------

    // ------------------------------
    //         MEMORY TRACKER
    // ------------------------------
  }   
}

class ReadDepend(p: PrevParams) extends Module {
  // Number of parallel registers by adding each PrevCtrl
  def nReg: Int = {
    var r: Int = 0
    
    for (np <- 0 until p.nPrevPort) {
      r = r + p.pPrev(np).nFieldSlct
    }

    return r
  }

  // Keep associated register number and original PrevCtrl number
  var nRegPrev = new Array[Int](nReg)
  for (np <- 0 until p.nPrevPort) {
    var r: Int = 0
    
    for (nds <- 0 until p.pPrev(np).nFieldSlct) {
      nRegPrev(r + nds) = np
    }

    r = r + p.pPrev(np).nFieldSlct
  }

  val io = IO(new Bundle {
    val i_acc = Input(Vec(nReg, new DependRegBus(p)))
    val o_dep_rw = Output(Vec(nReg, Bool()))
    val b_read = Vec(nReg, Flipped(new DependStageIO(p)))
    val i_ack = if (p.useAckReg && !p.readOnly) Some(Input(Vec(nReg, Input(new DependRegBus(p))))) else None
  })

  // ******************************
  //       CONFLICT FUNCTION 
  // ******************************
  def conflict (b0: DependRegBus, b1: DependRegBus): Bool = {
    val w_conflict = Wire(Bool())

    w_conflict := false.B
    if (p.useAccReg) {
      if (p.usePftch) {
        when (b0.pftch.get.use & b1.pftch.get.use) {
          when (b0.pftch.get.entry === b1.pftch.get.entry) {
            w_conflict := true.B
          }
        }.elsewhen(~b0.pftch.get.use & ~b1.pftch.get.use) {
          when ((b0.addr.set === b1.addr.set) & (b0.addr.line === b1.addr.line) & (b0.addr.data === b1.addr.data)) {
            w_conflict := true.B
          }
        }
      } else {
        when ((b0.addr.set === b1.addr.set) & (b0.addr.line === b1.addr.line) & (b0.addr.data === b1.addr.data)) {
          w_conflict := true.B
        }
      }
    } else {
      when ((b0.addr.tag === b1.addr.tag) & (b0.addr.set === b1.addr.set) & (b0.addr.data === b1.addr.data)) {
        w_conflict := true.B
      }
    }
    
    return w_conflict
  }

  // ******************************
  //            DEFAULT 
  // ******************************
  for (r <- 0 until nReg) {
    io.o_dep_rw(r) := false.B
    io.b_read(r).lock := false.B
  }

  // ******************************
  //           WRITE DEP
  // ******************************
  for (r0 <- 0 until nReg) {
    for (r1 <- 0 until nReg) {
      // ------------------------------
      //              BUS
      // ------------------------------
      if (r0 < r1) {
        when (io.i_acc(r0).valid & io.i_acc(r0).rw & conflict(io.i_acc(r0), io.i_acc(r1))) {
          io.o_dep_rw(r1) := true.B
        }
      }

      // ------------------------------
      //           READ STAGE
      // ------------------------------
      when (io.b_read(r0).reg.valid & io.b_read(r0).reg.rw & conflict(io.b_read(r0).reg, io.i_acc(r1))) {
        io.o_dep_rw(r1) := true.B
      }

      // ------------------------------
      //           ACK STAGE
      // ------------------------------
      if (p.useAckReg && !p.readOnly) {
        when (io.i_ack.get(r0).valid & io.i_ack.get(r0).rw & conflict(io.i_ack.get(r0), io.i_acc(r1))) {
          io.o_dep_rw(r1) := true.B
        }
      }    
    }
  }
}

object ReadStage extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new ReadStage(PrevUnitConfigBase), args)
}