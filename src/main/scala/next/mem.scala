/*
 * File: mem.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:14 pm                                       *
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
import herd.common.dome._
import herd.common.mem.mb4s._
import herd.mem.hay.common._
import herd.mem.hay.cache._


object MemStageFSM extends ChiselEnum {
  val s0NEW, s1REP = Value
}

class MemStage (p: NextParams) extends Module {
  import herd.mem.hay.next.MemStageFSM._

  val io = IO(new Bundle {
    val b_dome = if (p.useDome) Some(Vec(p.nDome, new DomeIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useDome, p.nDome, p.nTagBit, p.nSet)))
    
    val i_slct_prev = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val b_prev = Vec(p.nPrevPort, Flipped(new NextCtrlIO(p, p.nHart)))

    val b_port = new Mb4sReqIO(p.pNextBus)

    val i_slct_out = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
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
  //         DOME INTERFACE
  // ******************************
  val r_slct_prev = Reg(new SlctBus(p.nDome, p.nPart, 1))

  val w_slct_next = Wire(new SlctBus(p.nDome, p.nPart, 1))
  val w_slct_out = Wire(new SlctBus(p.nDome, p.nPart, 1))

  if (p.useDomeSlct) {
    r_slct_prev := io.i_slct_prev.get
    w_slct_next := r_slct_prev
    w_slct_out := io.i_slct_out.get
  } else {
    w_slct_next.dome := 0.U
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

    for (ds <- 0 until p.nDomeSlct) {
      m_mem.io.i_flush(ds) := false.B    

      m_mem.io.b_din(pp).valid(ds) := false.B
      if (p.useDomeTag) m_mem.io.b_din(pp).dome.get := io.b_prev(pp).dome.get      
      m_mem.io.b_din(pp).ctrl.get(ds).hart := io.b_prev(pp).hart
      m_mem.io.b_din(pp).ctrl.get(ds).rw := io.b_prev(pp).rw
      m_mem.io.b_din(pp).ctrl.get(ds).size := io.b_prev(pp).size
      m_mem.io.b_din(pp).ctrl.get(ds).addr := io.b_prev(pp).addr
    }
  }
  
  // Connect
  for (pp <- 0 until p.nPrevPort) {
    for (ds <- 0 until p.nDomeSlct) {
      if (p.useDomeSlct) {
        when (ds.U === io.b_prev(pp).dome.get) {
          w_wait_mem(pp) := ~m_mem.io.b_din(pp).ready(ds)
          m_mem.io.b_din(pp).valid(ds) := io.b_prev(pp).valid & ~w_wait_op(pp)
        }
      } else {
        w_wait_mem(pp) := ~m_mem.io.b_din(pp).ready(0)
        m_mem.io.b_din(pp).valid(0) := io.b_prev(pp).valid & ~w_wait_op(pp)
      }
    }
  }

  // Mux
  if (p.useDomeSlct) m_mem_mux.io.i_slct.get := w_slct_next
  m_mem_mux.io.b_din <> m_mem.io.b_dout(0)

  // ------------------------------
  //           NEXT PORT
  // ------------------------------ 
  val r_fsm = RegInit(VecInit(Seq.fill(p.nDomeSlct)(s0NEW)))
  val w_cfsm = Wire(MemStageFSM())
  val w_nfsm = Wire(MemStageFSM())

  val r_data = Reg(Vec(p.nDomeSlct, UInt(log2Ceil(p.nData).W)))
  val w_cdata = Wire(UInt(log2Ceil(p.nData).W))
  val w_ndata = Wire(UInt(log2Ceil(p.nData).W))
  val w_cmem = Wire(new GenSVBus(p, new NextMemCtrlBus(p), UInt(0.W)))

  val w_rep_last = (w_cdata === (p.nData - 1).U)

  // Current values
  w_cfsm := r_fsm(w_slct_next.dome)

  w_cmem.valid := m_mem_mux.io.b_sout.valid
  if (p.useDome) w_cmem.dome.get := m_mem_mux.io.b_sout.dome.get
  w_cmem.ctrl.get := m_mem_mux.io.b_sout.ctrl.get

  when ((w_cfsm === s0NEW) & w_cmem.ctrl.get.rw) {
    w_cdata := w_cmem.ctrl.get.addr.data
  }.otherwise {
    w_cdata := r_data(w_slct_next.dome)
  }    

  // Next values
  w_nfsm := w_cfsm
  w_ndata := w_cdata
  
  // Update registers
  r_data(w_slct_next.dome) := w_ndata
  r_fsm(w_slct_next.dome) := w_nfsm

  // FSM
  switch(w_cfsm) {
    is (s0NEW) {
      w_ndata := 0.U
      when (w_cmem.valid & ~w_cmem.ctrl.get.rw & io.b_port.ready(w_slct_next.dome)) {
        if (p.nRepCycle > 1) {
          w_nfsm := s1REP
          w_ndata := 1.U
        }
      }
    }

    is (s1REP) {
      when (io.b_port.ready(w_slct_next.dome)) {        
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
  for (ds <- 0 until p.nDomeSlct) {
    m_mem_mux.io.b_sout.ready := io.b_port.ready(w_slct_next.dome) & (((w_cfsm === s0NEW) & w_cmem.ctrl.get.rw) | w_rep_last)
  }

  // Next port
  io.b_port.valid := w_cmem.valid
  if (p.useDome) io.b_port.dome.get := w_cmem.dome.get
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

    for (ds <- 0 until p.nDomeSlct) {
      m_op.io.i_flush(ds) := false.B    

      m_op.io.b_din(pp).valid(ds) := false.B
      if (p.useDomeTag) m_op.io.b_din(pp).dome.get := io.b_prev(pp).dome.get
      m_op.io.b_din(pp).ctrl.get(ds).prev := pp.U
      m_op.io.b_din(pp).ctrl.get(ds).rw := io.b_prev(pp).rw
      m_op.io.b_din(pp).ctrl.get(ds).addr := io.b_prev(pp).addr
    }
  }

  // Connect  
  for (pp <- 0 until p.nPrevPort) {
    for (ds <- 0 until p.nDomeSlct) {
      if (p.useDomeSlct) {
        when (ds.U === io.b_prev(pp).dome.get) {
          w_wait_op(pp) := ~m_op.io.b_din(pp).ready(ds)
          m_op.io.b_din(pp).valid(ds) := io.b_prev(pp).valid & ~w_wait_mem(pp)
        }
      } else {
        w_wait_op(pp) := ~m_op.io.b_din(pp).ready(0)
        m_op.io.b_din(pp).valid(0) := io.b_prev(pp).valid & ~w_wait_mem(pp)
      }
    }
  }

  // Outputs
  if (p.useDomeSlct) m_op_mux.io.i_slct.get := w_slct_out
  m_op_mux.io.b_din <> m_op.io.b_dout(0)
  io.b_out <> m_op_mux.io.b_sout
     
  // ******************************
  //              CBO
  // ******************************
  for (c <- 0 until p.nCbo) {
    io.b_cbo(c).ready := io.b_cbo(c).valid

    when (io.b_cbo(c).valid & io.b_cbo(c).inv) {
      if (p.useDomeSlct) {
        for (d <- 0 until p.nDome) {
          when (d.U === io.b_cbo(c).dome.get) {
            io.b_cbo(c).ready := ~m_op.io.o_val(0).valid(d)
          }
        }
      } else if (p.useDomeTag) {
        for (fd <- 0 until p.nNextFifoDepth) {
          when (m_op.io.o_val(fd).valid(0) & (io.b_cbo(c).dome.get === m_op.io.o_val(fd).dome.get)) {
            io.b_cbo(c).ready := false.B
          }
        }
      } else {
        io.b_cbo(c).ready := ~m_op.io.o_val(0).valid(0)
      }
    }    
  }

  // ******************************
  //             DOME
  // ******************************
  for (d <- 0 until p.nDome) {
    if (p.useDomeSlct) {
      io.b_dome.get(d).free := ~m_op.io.o_val(0).valid(d)
    } else if (p.useDomeTag) {
      io.b_dome.get(d).free := true.B
      for (fd <- 0 until p.nNextFifoDepth) {
        when (m_op.io.o_val(fd).valid(0) & (d.U === m_op.io.o_val(fd).dome.get)) {
          io.b_dome.get(d).free := false.B
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
    for (ds <- 0 until p.nDomeSlct) {
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
      for (ds <- 0 until p.nDomeSlct) {
        m_mem.io.b_din(pp).ctrl.get(ds).mtd.get.addr := io.b_prev(pp).addr.toFull()
        m_mem.io.b_din(pp).ctrl.get(ds).mtd.get.mem := io.b_prev(pp).addr.mem()
        m_op.io.b_din(pp).ctrl.get(ds).mtd.get.addr := io.b_prev(pp).addr.toFull()
        m_op.io.b_din(pp).ctrl.get(ds).mtd.get.mem := io.b_prev(pp).addr.mem()
      }
    }
  }
}

object MemStage extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new MemStage(NextConfigBase), args)
}
