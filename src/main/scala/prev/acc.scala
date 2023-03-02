/*
 * File: acc.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-02 01:45:05 pm                                       *
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
import chisel3.experimental.ChiselEnum

import herd.common.gen._
import herd.common.tools._
import herd.common.field._
import herd.common.mem.mb4s._
import herd.mem.hay.common._
import herd.mem.hay.cache._
import herd.mem.hay.next._
import herd.mem.hay.pftch.{PftchAccIO}


object AccStageFSM extends ChiselEnum {
  val s0NEW, s1REP, s2ACK, s3WAIT = Value
}

class AccStage (p: PrevUnitParams) extends Module {
  import herd.mem.hay.prev.AccStageFSM._

  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useField, p.nField, p.nTagBit, p.nSet)))

    val i_slct = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_in =  Flipped(new GenSRVIO(p, new PrevUnitCtrlBus(p), UInt(0.W)))

    val b_acc = new CacheAccIO(p)
    val b_rep = new CacheRepIO(p)
    val b_pftch = if (p.usePftch) Some(new PftchAccIO(p, p.nPftchEntry)) else None
    val b_rsv = if (p.useAmo) Some(Flipped(new ReserveIO(p.nHart, p.nAddrBit))) else None

    val b_next = new NextCtrlIO(p, p.nHart)
    val o_miss = Output(new MissBus(p, p.nHart))

    val b_dep = if (p.useAccReg) Some(Vec(p.nFieldSlct, new DependStageIO(p))) else None
    val o_pend = if (p.useAccReg) Some(Output(Vec(p.nFieldSlct, new CachePendBus(p)))) else None
  
    val o_slct = if (p.useFieldSlct) Some(Output(new SlctBus(p.nField, p.nPart, 1))) else None  
    val b_out = new GenSRVIO(p, new PrevUnitCtrlBus(p), UInt(0.W))
  })

  val m_reg = if (p.useAccReg) Some(Module(new GenSReg(p, new PrevUnitCtrlBus(p), UInt(0.W), false, false, true))) else None

  // ******************************
  //            STATUS
  // ******************************
  val r_fsm = RegInit(VecInit(Seq.fill(p.nFieldSlct)(s0NEW)))
  val w_cfsm = Wire(AccStageFSM())
  val w_nfsm = Wire(AccStageFSM())

  val w_wait_reg = Wire(Bool())
  val w_wait_next = Wire(Bool())

  val w_write = Wire(Bool())

  if (p.useAmo) {
    w_write := io.b_in.ctrl.get.op.wa & ((io.b_in.ctrl.get.op.op =/= OP.SC) | io.b_rsv.get.ready)
  } else {
    w_write := io.b_in.ctrl.get.op.wo
  }

  // ******************************
  //           INTERFACE
  // ******************************
  // ------------------------------
  //            FIELD
  // ------------------------------
  val r_slct_out = Reg(new SlctBus(p.nField, p.nPart, 1))

  val w_slct_in = Wire(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_out = Wire(new SlctBus(p.nField, p.nPart, 1))

  if (p.useFieldSlct) {    
    w_slct_in := io.i_slct.get
    if (p.useAccReg) {
      r_slct_out := w_slct_in
      w_slct_out := r_slct_out
    } else {
      w_slct_out := w_slct_in
    }
    io.o_slct.get := w_slct_out
  } else {
    w_slct_in.field := 0.U
    w_slct_in.next := 0.U
    w_slct_in.step := 0.U
    w_slct_out := w_slct_in   
  }

  // ------------------------------
  //          PREFETCHER
  // ------------------------------
  val w_pftch_ready = Wire(Bool())
  val w_pftch_found = Wire(Bool())
  val w_pftch_av = Wire(Bool())

  if (p.usePftch) {
    w_pftch_ready := io.b_pftch.get.ready
    w_pftch_found := io.b_pftch.get.found
    w_pftch_av := io.b_pftch.get.av
  } else {
    w_pftch_ready := true.B
    w_pftch_found := false.B
    w_pftch_av := false.B
  }

  // ******************************
  //              FSM
  // ******************************
  val w_is_rep = Wire(Bool())

  val w_data_miss = Wire(Bool())
  val w_data_here = Wire(Bool())
  val w_data_av = Wire(Bool())
  val w_data_wait = Wire(Bool())
  val w_data_pftch = Wire(Bool())

  w_is_rep := (w_cfsm === s1REP) | (w_cfsm === s2ACK)

  w_data_miss := io.b_acc.ready & ~io.b_acc.found & w_pftch_ready & ~w_pftch_found
  w_data_here := (io.b_acc.ready & io.b_acc.found) | (w_pftch_ready & w_pftch_found)
  w_data_av := (io.b_acc.ready & io.b_acc.found & io.b_acc.av) | (w_pftch_ready & w_pftch_found & w_pftch_av)
  w_data_wait := (io.b_acc.ready & io.b_acc.found & ~io.b_acc.av) | (w_pftch_ready & w_pftch_found & ~w_pftch_av)
  w_data_pftch := io.b_acc.ready & ~io.b_acc.found & w_pftch_ready & w_pftch_found

  w_cfsm := r_fsm(w_slct_in.field)
  r_fsm(w_slct_in.field) := w_nfsm

  w_nfsm := w_cfsm

  switch (w_cfsm) {
    is (s0NEW) {
      when (io.b_in.valid & w_data_miss) {
        w_nfsm := s1REP
      }.elsewhen(io.b_in.valid & w_data_wait) {
        w_nfsm := s3WAIT
      }
    }

    is (s1REP) {
      when (io.b_rep.ready & (io.b_rep.found | (io.b_rep.alloc & ~(w_wait_next | w_wait_reg)))) {
        w_nfsm := s3WAIT
      }.elsewhen (io.b_rep.ready & io.b_rep.alloc & (w_wait_next | w_wait_reg)) {
        w_nfsm := s2ACK
      }
    }

    is (s2ACK) {
      when (~(w_wait_next | w_wait_reg)) {
        w_nfsm := s3WAIT   
      }
    }

    is (s3WAIT) {
      when (io.b_in.valid & w_data_av & ~(w_wait_next | w_wait_reg)) {
        w_nfsm := s0NEW
      }
    }
  }

  // ******************************
  //             CACHE
  // ******************************
  // ------------------------------
  //             ACCESS
  // ------------------------------
  io.b_acc.valid := ((w_cfsm === s0NEW) & io.b_in.valid) | (w_cfsm === s3WAIT)
  io.b_acc.hart := io.b_in.ctrl.get.info.hart
  io.b_acc.rw := io.b_in.ctrl.get.op.wa
  if (p.useField) io.b_acc.field.get := io.b_in.field.get
  io.b_acc.tag := io.b_in.ctrl.get.addr.tag
  io.b_acc.set := io.b_in.ctrl.get.addr.set

  // ------------------------------
  //            REPLACE
  // ------------------------------
  val r_rep_line = Reg(Vec(p.nFieldSlct, UInt(log2Ceil(p.nLine).W)))
  val w_rep_line = Wire(UInt(log2Ceil(p.nLine).W))

  io.b_rep.valid := (w_cfsm === s1REP)
  io.b_rep.hart := io.b_in.ctrl.get.info.hart
  if (p.useField) io.b_rep.field.get := io.b_in.field.get
  io.b_rep.check := false.B
  io.b_rep.empty := false.B
  io.b_rep.tag := io.b_in.ctrl.get.addr.tag
  io.b_rep.set := io.b_in.ctrl.get.addr.set

  when (w_cfsm === s1REP) {
    r_rep_line(w_slct_in.field) := io.b_rep.line
    w_rep_line := io.b_rep.line
  }.otherwise {
    w_rep_line := r_rep_line(w_slct_in.field)
  }

  // ******************************
  //           PREFETCHER
  // ******************************
  val r_pftch_in_use = RegInit(VecInit(Seq.fill(p.nFieldSlct)(false.B)))

  if (p.usePftch) {
    io.b_pftch.get.valid := ((w_cfsm === s0NEW) & io.b_in.valid) | (w_cfsm === s3WAIT)
    io.b_pftch.get.hart := io.b_in.ctrl.get.info.hart
    io.b_pftch.get.rw := io.b_in.ctrl.get.op.wa
    io.b_pftch.get.check := r_pftch_in_use(w_slct_in.field) | (w_cfsm === s3WAIT)
    if (p.useField) io.b_pftch.get.field.get := io.b_in.field.get
    io.b_pftch.get.tag := io.b_in.ctrl.get.addr.tag
    io.b_pftch.get.set := io.b_in.ctrl.get.addr.set

    when (r_pftch_in_use(w_slct_in.field)) {
      r_pftch_in_use(w_slct_in.field) := (w_wait_reg | w_data_wait)
    }.otherwise {
      r_pftch_in_use(w_slct_in.field) := io.b_in.valid & io.b_pftch.get.found & (w_wait_reg | w_data_wait)
    }
  }

  // ******************************
  //            RESERVE
  // ******************************
  if (p.useAmo) {    
    io.b_rsv.get.valid := io.b_in.valid
    io.b_rsv.get.hart := io.b_in.ctrl.get.info.hart
    io.b_rsv.get.rw := io.b_in.ctrl.get.op.wa
    io.b_rsv.get.rsv := (io.b_in.ctrl.get.op.op === OP.LR)
    io.b_rsv.get.size := io.b_in.ctrl.get.op.size
    io.b_rsv.get.addr := io.b_in.ctrl.get.addr.toFull()
  }

  // ******************************
  //              BUS
  // ******************************
  val w_bus = Wire(new GenSVBus(p, new PrevUnitCtrlBus(p), UInt(0.W)))

  w_bus.valid := io.b_in.valid & ~w_is_rep & w_data_av & ~w_wait_next
  if (p.useField) w_bus.field.get := io.b_in.field.get
  w_bus.ctrl.get := io.b_in.ctrl.get
  if (p.useAmo) w_bus.ctrl.get.info.rsv.get := io.b_rsv.get.ready
  w_bus.ctrl.get.addr.line := io.b_acc.line
  if (p.usePftch) {
    w_bus.ctrl.get.pftch.get.use := w_data_pftch
    w_bus.ctrl.get.pftch.get.entry := io.b_pftch.get.entry
  }  

  // ******************************
  //        NEXT CONTROLLER
  // ******************************
  val w_next_write = Wire(Bool())
  val w_next_rep = Wire(Bool())

  w_wait_next := ~io.b_next.ready

  if (!p.readOnly) {
    w_next_write := (((w_cfsm === s0NEW) & io.b_in.valid) | (w_cfsm === s3WAIT)) & w_write & w_data_av
  } else {
    w_next_write := false.B
  }  
  w_next_rep := (w_cfsm === s2ACK) | ((w_cfsm === s1REP) & io.b_rep.ready & io.b_rep.alloc & ~io.b_rep.found)

  io.b_next.valid := ((~w_wait_reg & w_next_write) | w_next_rep)
  if (p.useField) io.b_next.field.get := io.b_in.field.get
  io.b_next.hart := io.b_in.ctrl.get.info.hart
  if (!p.readOnly) io.b_next.rw := ~w_is_rep & io.b_in.ctrl.get.op.wa else io.b_next.rw := false.B
  io.b_next.size := io.b_in.ctrl.get.op.size
  io.b_next.addr := io.b_in.ctrl.get.addr
  io.b_next.addr.line := w_rep_line

  // ******************************
  //           REGISTERS
  // ******************************
  io.b_in.ready := ~w_is_rep & ~w_wait_reg & ~w_wait_next & w_data_av

  if (p.useAccReg) {
    w_wait_reg := ~m_reg.get.io.b_sin.ready

    for (fs <- 0 until p.nFieldSlct) {
      m_reg.get.io.i_flush(fs) := false.B
    }  

    if (p.useFieldSlct) {
      m_reg.get.io.i_slct_in.get := w_slct_in
      m_reg.get.io.i_slct_out.get := w_slct_out
    }

    m_reg.get.io.b_sin.valid := w_bus.valid
    if (p.useField) m_reg.get.io.b_sin.field.get := w_bus.field.get
    m_reg.get.io.b_sin.ctrl.get := w_bus.ctrl.get

    io.b_out <> m_reg.get.io.b_sout
    for (fs <- 0 until p.nFieldSlct) {
      when (fs.U === w_slct_out.field) {
        m_reg.get.io.b_sout.ready := io.b_out.ready & ~io.b_dep.get(fs).lock
        io.b_out.valid := m_reg.get.io.b_sout.valid & ~io.b_dep.get(fs).lock
      }
    }

  } else {
    w_wait_reg := ~io.b_out.ready

    io.b_out.valid := w_bus.valid
    if (p.useField) io.b_out.field.get := w_bus.field.get
    io.b_out.ctrl.get := w_bus.ctrl.get
  }

  // Force simplification during design generation
  if (p.readOnly) io.b_out.ctrl.get.op.op := OP.R
  
  // ******************************
  //             DEPEND
  // ******************************  
  if (p.useAccReg) {
    for (fs <- 0 until p.nFieldSlct) {
      io.b_dep.get(fs).reg := DontCare
      io.b_dep.get(fs).reg.valid := m_reg.get.io.o_val.valid(fs)
      io.b_dep.get(fs).reg.lock := (fs.U =/= w_slct_out.field) | ~io.b_out.ready
      io.b_dep.get(fs).reg.addr := m_reg.get.io.o_val.ctrl.get(fs).addr
      if (p.useFieldSlct) {
        io.b_dep.get(fs).reg.field.get := fs.U
      } else if (p.useField) {
        io.b_dep.get(0).reg.field.get := m_reg.get.io.o_val.field.get
      }
    }
  }
  
  // ******************************
  //             PEND
  // ******************************
  if (p.useAccReg) {
    for (fs <- 0 until p.nFieldSlct) {
      if (p.usePftch) {
        io.o_pend.get(fs).valid := m_reg.get.io.o_val.valid(fs) & ~m_reg.get.io.o_val.ctrl.get(fs).pftch.get.use
      } else {
        io.o_pend.get(fs).valid := m_reg.get.io.o_val.valid(fs)
      }   
      io.o_pend.get(fs).valid := m_reg.get.io.o_val.valid(fs)
      io.o_pend.get(fs).line := m_reg.get.io.o_val.ctrl.get(fs).addr.line
      io.o_pend.get(fs).set := m_reg.get.io.o_val.ctrl.get(fs).addr.set
    }
  }

  // ******************************
  //             MISS
  // ******************************
  val init_miss = Wire(new MissBus(p, p.nHart))
  init_miss := DontCare
  init_miss.valid := false.B

  val r_miss = RegInit(init_miss)

  r_miss.valid := (w_cfsm === s0NEW) & io.b_in.valid & w_data_miss
  r_miss.hart := io.b_in.ctrl.get.info.hart
  if (p.useField) r_miss.field.get := io.b_in.field.get
  r_miss.addr := io.b_in.ctrl.get.addr

  io.o_miss := r_miss

  // ******************************
  //            CBO
  // ******************************
  for (c <- 0 until p.nCbo) {
    io.b_cbo(c).ready := io.b_cbo(c).valid

    if (p.useAccReg) {
      when (io.b_cbo(c).valid & io.b_cbo(c).inv) {
        if (p.useFieldSlct) {
          for (f <- 0 until p.nField) {
            when (f.U === io.b_cbo(c).field.get) {
              io.b_cbo(c).ready := ~m_reg.get.io.o_val.valid(f)
            }
          }
        } else if (p.useFieldTag) {
          when (io.b_cbo(c).field.get === m_reg.get.io.o_val.field.get) {
            io.b_cbo(c).ready := ~m_reg.get.io.o_val.valid(0)
          }
        } else {
          io.b_cbo(c).ready := ~m_reg.get.io.o_val.valid(0)
        }
      }
    }
  }

  // ******************************
  //            FIELD
  // ******************************
  if (p.useField) {
    if (p.useAccReg) {
      if (p.useFieldSlct) {
        for (f <- 0 until p.nField) {
          io.b_field.get(f).free := ~m_reg.get.io.o_val.valid(f)
        } 
      } else {
        for (f <- 0 until p.nField) {
          io.b_field.get(f).free := (f.U =/= m_reg.get.io.o_val.field.get) | ~m_reg.get.io.o_val.valid(0)
        }    
      }      
    } else {
      for (f <- 0 until p.nField) {
        io.b_field.get(f).free := true.B
      }
    }
  } 

  // ******************************
  //            DEBUG
  // ******************************
  if (p.debug) {
    // ------------------------------
    //            SIGNALS
    // ------------------------------
    dontTouch(io.b_next)
    dontTouch(w_next_rep)
    dontTouch(w_next_write)
    dontTouch(w_wait_next)

    // ------------------------------
    //         DATA FOOTPRINT
    // ------------------------------

    // ------------------------------
    //         MEMORY TRACKER
    // ------------------------------
  }
}

class AccDepend(p: PrevParams) extends Module {
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
    val b_acc = Vec(nReg, Flipped(new DependStageIO(p)))
    val i_read = Input(Vec(nReg, new DependRegBus(p)))
  })

  // ******************************
  //            DEFAULT 
  // ******************************
  for (r <- 0 until nReg) {
    io.b_acc(r).lock := false.B
  }  

  // ******************************
  //          VERTICAL DEP 
  // ******************************  
  if (p.useAccReg) {
    for (r0 <- 0 until nReg) {
      for (r1 <- (r0 + 1) until nReg) {
        val w_field = Wire(Bool())

        if (p.useField) {
          w_field := (io.i_read(r0).field.get === io.b_acc(r1).reg.field.get)
        } else {
          w_field := true.B
        }

        when (io.i_read(r1).valid & io.i_read(r1).lock & w_field) {
          io.b_acc(r0).lock := true.B
        }
      }
    }
  }

  // ******************************
  //         HORIZONTAL DEP 
  // ******************************  
  if (p.useAccReg) {
    for (r0 <- 0 until nReg) {
      for (r1 <- 0 until r0) {
        val w_field = Wire(Bool())

        if (p.useField) {
          w_field := (io.b_acc(r0).reg.field.get === io.b_acc(r1).reg.field.get)
        } else {
          w_field := true.B
        }

        when (io.b_acc(r1).reg.valid & io.b_acc(r1).reg.lock & w_field) {
          io.b_acc(r0).lock := true.B
        }
      }
    }
  }
}

object AccStage extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new AccStage(PrevUnitConfigBase), args)
}
