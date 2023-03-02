/*
 * File: mem.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-02 01:40:10 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay.next

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

import herd.common.gen._
import herd.common.tools._
import herd.common.field._
import herd.common.mem.mb4s._
import herd.mem.hay.common._
import herd.mem.hay.cache._


object MemStageFSM extends ChiselEnum {
  val s0NEW, s1REP = Value
}

class MemStage (p: NextParams) extends Module {
  import herd.mem.hay.next.MemStageFSM._

  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useField, p.nField, p.nTagBit, p.nSet)))
    
    val i_slct_prev = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_prev = Vec(p.nPrevPort, Flipped(new NextCtrlIO(p, p.nHart)))

    val b_port = new Mb4sReqIO(p.pNextBus)

    val i_slct_out = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_out = new GenSRVIO(p, new NextOpCtrlBus(p), UInt(0.W))
  })

  val m_mem = Module(new GenDFifo(p, new NextMemCtrlBus(p), UInt(0.W), 2, p.nNextFifoDepth, p.nPrevPort, 1))
  val m_mem_mux = Module(new GenSMux(p, new NextMemCtrlBus(p), UInt(0.W)))
  val m_op = Module(new GenDFifo(p, new NextOpCtrlBus(p), UInt(0.W), 2, p.nNextFifoDepth, p.nPrevPort, 1))
  val m_op_mux = Module(new GenSMux(p, new NextOpCtrlBus(p), UInt(0.W)))

  // ******************************
  //            STATUS
  // ******************************
  val w_wait_mem = Wire(Vec(p.nPrevPort, Bool()))
  val w_wait_op = Wire(Vec(p.nPrevPort, Bool()))

  for (pp <- 0 until p.nPrevPort) {
    io.b_prev(pp).ready := ~w_wait_mem(pp) & ~w_wait_op(pp)
  }  

  // ******************************
  //        FIELD INTERFACE
  // ******************************
  val r_slct_prev = Reg(new SlctBus(p.nField, p.nPart, 1))

  val w_slct_next = Wire(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_out = Wire(new SlctBus(p.nField, p.nPart, 1))

  if (p.useFieldSlct) {
    r_slct_prev := io.i_slct_prev.get
    w_slct_next := r_slct_prev
    w_slct_out := io.i_slct_out.get
  } else {
    w_slct_next.field := 0.U
    w_slct_next.next := 0.U
    w_slct_next.step := 0.U
    r_slct_prev := w_slct_next
    w_slct_out := w_slct_next
  }

  // ******************************
  //          NEXT MEMORY
  // ******************************
  // ------------------------------
  //          PREV REQUEST
  // ------------------------------  
  // Default
  for (pp <- 0 until p.nPrevPort) {
    w_wait_mem(pp) := false.B

    for (fs <- 0 until p.nFieldSlct) {
      m_mem.io.i_flush(fs) := false.B    

      m_mem.io.b_din(pp).valid(fs) := false.B
      if (p.useFieldTag) m_mem.io.b_din(pp).field.get := io.b_prev(pp).field.get      
      m_mem.io.b_din(pp).ctrl.get(fs).hart := io.b_prev(pp).hart
      m_mem.io.b_din(pp).ctrl.get(fs).rw := io.b_prev(pp).rw
      m_mem.io.b_din(pp).ctrl.get(fs).size := io.b_prev(pp).size
      m_mem.io.b_din(pp).ctrl.get(fs).addr := io.b_prev(pp).addr
    }
  }
  
  // Connect
  for (pp <- 0 until p.nPrevPort) {
    for (fs <- 0 until p.nFieldSlct) {
      if (p.useFieldSlct) {
        when (fs.U === io.b_prev(pp).field.get) {
          w_wait_mem(pp) := ~m_mem.io.b_din(pp).ready(fs)
          m_mem.io.b_din(pp).valid(fs) := io.b_prev(pp).valid & ~w_wait_op(pp)
        }
      } else {
        w_wait_mem(pp) := ~m_mem.io.b_din(pp).ready(0)
        m_mem.io.b_din(pp).valid(0) := io.b_prev(pp).valid & ~w_wait_op(pp)
      }
    }
  }

  // Mux
  if (p.useFieldSlct) m_mem_mux.io.i_slct.get := w_slct_next
  m_mem_mux.io.b_din <> m_mem.io.b_dout(0)

  // ------------------------------
  //           NEXT PORT
  // ------------------------------ 
  val r_fsm = RegInit(VecInit(Seq.fill(p.nFieldSlct)(s0NEW)))
  val w_cfsm = Wire(MemStageFSM())
  val w_nfsm = Wire(MemStageFSM())

  val r_data = Reg(Vec(p.nFieldSlct, UInt(log2Ceil(p.nData).W)))
  val w_cdata = Wire(UInt(log2Ceil(p.nData).W))
  val w_ndata = Wire(UInt(log2Ceil(p.nData).W))
  val w_cmem = Wire(new GenSVBus(p, new NextMemCtrlBus(p), UInt(0.W)))

  val w_rep_last = (w_cdata === (p.nData - 1).U)

  // Current values
  w_cfsm := r_fsm(w_slct_next.field)

  w_cmem.valid := m_mem_mux.io.b_sout.valid
  if (p.useField) w_cmem.field.get := m_mem_mux.io.b_sout.field.get
  w_cmem.ctrl.get := m_mem_mux.io.b_sout.ctrl.get

  when ((w_cfsm === s0NEW) & w_cmem.ctrl.get.rw) {
    w_cdata := w_cmem.ctrl.get.addr.data
  }.otherwise {
    w_cdata := r_data(w_slct_next.field)
  }    

  // Next values
  w_nfsm := w_cfsm
  w_ndata := w_cdata
  
  // Update registers
  r_data(w_slct_next.field) := w_ndata
  r_fsm(w_slct_next.field) := w_nfsm

  // FSM
  switch(w_cfsm) {
    is (s0NEW) {
      w_ndata := 0.U
      when (w_cmem.valid & ~w_cmem.ctrl.get.rw & io.b_port.ready(w_slct_next.field)) {
        if (p.nRepCycle > 1) {
          w_nfsm := s1REP
          w_ndata := 1.U
        }
      }
    }

    is (s1REP) {
      when (io.b_port.ready(w_slct_next.field)) {        
        when (w_rep_last) {
          w_nfsm := s0NEW
          w_ndata := 0.U
        }.otherwise {
          w_ndata := w_cdata + 1.U
        }
      } 
    }
  }

  // Read mem buffer
  for (fs <- 0 until p.nFieldSlct) {
    m_mem_mux.io.b_sout.ready := io.b_port.ready(w_slct_next.field) & (((w_cfsm === s0NEW) & w_cmem.ctrl.get.rw) | w_rep_last)
  }

  // Next port
  io.b_port.valid := w_cmem.valid
  if (p.useField) io.b_port.field.get := w_cmem.field.get
  io.b_port.ctrl.hart := w_cmem.ctrl.get.hart
  io.b_port.ctrl.op := Mux(w_cmem.ctrl.get.rw, OP.W, OP.R)
  io.b_port.ctrl.size := Mux(w_cmem.ctrl.get.rw, w_cmem.ctrl.get.size, SIZE.toSize(p.nNextDataByte).U)
  io.b_port.ctrl.addr := Mux(w_cmem.ctrl.get.rw, w_cmem.ctrl.get.addr.toFull(), Cat(w_cmem.ctrl.get.addr.tag, w_cmem.ctrl.get.addr.set, w_cdata, 0.U(log2Ceil(p.nDataByte).W)))
  
  // ******************************
  //          SELECT OP
  // ******************************    
  // Default
  for (pp <- 0 until p.nPrevPort) {
    w_wait_op(pp) := false.B

    for (fs <- 0 until p.nFieldSlct) {
      m_op.io.i_flush(fs) := false.B    

      m_op.io.b_din(pp).valid(fs) := false.B
      if (p.useFieldTag) m_op.io.b_din(pp).field.get := io.b_prev(pp).field.get
      m_op.io.b_din(pp).ctrl.get(fs).prev := pp.U
      m_op.io.b_din(pp).ctrl.get(fs).rw := io.b_prev(pp).rw
      m_op.io.b_din(pp).ctrl.get(fs).addr := io.b_prev(pp).addr
    }
  }

  // Connect  
  for (pp <- 0 until p.nPrevPort) {
    for (fs <- 0 until p.nFieldSlct) {
      if (p.useFieldSlct) {
        when (fs.U === io.b_prev(pp).field.get) {
          w_wait_op(pp) := ~m_op.io.b_din(pp).ready(fs)
          m_op.io.b_din(pp).valid(fs) := io.b_prev(pp).valid & ~w_wait_mem(pp)
        }
      } else {
        w_wait_op(pp) := ~m_op.io.b_din(pp).ready(0)
        m_op.io.b_din(pp).valid(0) := io.b_prev(pp).valid & ~w_wait_mem(pp)
      }
    }
  }

  // Outputs
  if (p.useFieldSlct) m_op_mux.io.i_slct.get := w_slct_out
  m_op_mux.io.b_din <> m_op.io.b_dout(0)
  io.b_out <> m_op_mux.io.b_sout
     
  // ******************************
  //              CBO
  // ******************************
  for (c <- 0 until p.nCbo) {
    io.b_cbo(c).ready := io.b_cbo(c).valid

    when (io.b_cbo(c).valid & io.b_cbo(c).inv) {
      if (p.useFieldSlct) {
        for (f <- 0 until p.nField) {
          when (f.U === io.b_cbo(c).field.get) {
            io.b_cbo(c).ready := ~m_op.io.o_val(0).valid(f)
          }
        }
      } else if (p.useFieldTag) {
        for (fd <- 0 until p.nNextFifoDepth) {
          when (m_op.io.o_val(fd).valid(0) & (io.b_cbo(c).field.get === m_op.io.o_val(fd).field.get)) {
            io.b_cbo(c).ready := false.B
          }
        }
      } else {
        io.b_cbo(c).ready := ~m_op.io.o_val(0).valid(0)
      }
    }    
  }

  // ******************************
  //            FIELD
  // ******************************
  for (f <- 0 until p.nField) {
    if (p.useFieldSlct) {
      io.b_field.get(f).free := ~m_op.io.o_val(0).valid(f)
    } else if (p.useFieldTag) {
      io.b_field.get(f).free := true.B
      for (fd <- 0 until p.nNextFifoDepth) {
        when (m_op.io.o_val(fd).valid(0) & (f.U === m_op.io.o_val(fd).field.get)) {
          io.b_field.get(f).free := false.B
        }
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
    dontTouch(w_wait_mem)
    dontTouch(w_wait_op)
    dontTouch(io.b_prev)
    dontTouch(w_cmem)
    for (fs <- 0 until p.nFieldSlct) {
      dontTouch(m_mem.io.b_din)
      dontTouch(m_op.io.b_din)
    }    

    // ------------------------------
    //         DATA FOOTPRINT
    // ------------------------------

    // ------------------------------
    //         MEMORY TRACKER
    // ------------------------------
    for (pp <- 0 until p.nPrevPort) {
      for (fs <- 0 until p.nFieldSlct) {
        m_mem.io.b_din(pp).ctrl.get(fs).mtd.get.addr := io.b_prev(pp).addr.toFull()
        m_mem.io.b_din(pp).ctrl.get(fs).mtd.get.mem := io.b_prev(pp).addr.mem()
        m_op.io.b_din(pp).ctrl.get(fs).mtd.get.addr := io.b_prev(pp).addr.toFull()
        m_op.io.b_din(pp).ctrl.get(fs).mtd.get.mem := io.b_prev(pp).addr.mem()
      }
    }
  }
}

object MemStage extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new MemStage(NextConfigBase), args)
}
