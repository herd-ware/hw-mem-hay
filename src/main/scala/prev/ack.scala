/*
 * File: ack.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-02 01:45:09 pm                                       *
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
import herd.common.mem.mb4s._
import herd.mem.hay.common._
import herd.mem.hay.cache._
import herd.mem.hay.pftch.{PftchWriteIO}


class AckStage(p: PrevUnitParams) extends Module {
  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useField, p.nField, p.nTagBit, p.nSet)))
    
    val i_slct = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_in =  Flipped(new GenSRVIO(p, new PrevUnitCtrlBus(p), Vec(p.nDataByte, UInt(8.W))))   
    
    val b_port = Flipped(new Mb4sAckIO(p.pPrevBus))
    val b_next = if (!p.readOnly) Some(new GenDRVIO(p, UInt(0.W), UInt((p.nDataByte * 8).W))) else None

    val o_dep = if (p.useAckReg && !p.readOnly) Some(Output(Vec(p.nFieldSlct, new DependRegBus(p)))) else None
    val o_pend = if (p.useAckReg && !p.readOnly) Some(Output(Vec(p.nFieldSlct, new CachePendBus(p)))) else None

    val o_slct = if (p.useFieldSlct) Some(Output(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_out = if (!p.readOnly) Some(new GenSRVIO(p, new PrevUnitCtrlBus(p), new WriteDataBus(p))) else None
  })

  val m_reg = if (!p.readOnly && p.useAckReg) Some(Module(new GenSReg(p, new PrevUnitCtrlBus(p), new WriteDataBus(p), false, false, true))) else None

  val m_wport = if (!p.readOnly) Some(Module(new Mb4sDataSReg(p.pPrevBus))) else None
  val r_rport = RegInit(VecInit(Seq.fill(p.nFieldSlct){true.B}))

  // ******************************
  //            STATUS
  // ******************************
  val w_wait_port = Wire(Bool())
  val w_wait_next = Wire(Bool())
  val w_wait_reg = Wire(Bool())

  val w_sc_valid = Wire(Bool())
  val w_sc_rsv = Wire(Bool())
  val w_sc_write = Wire(Bool())

  if (p.useAmo) {
    w_sc_valid := (io.b_in.ctrl.get.op.op === OP.SC)
    w_sc_rsv := w_sc_valid & io.b_in.ctrl.get.info.rsv.get
    w_sc_write := ~w_sc_valid | w_sc_rsv
  } else {
    w_sc_valid := false.B
    w_sc_rsv := false.B
    w_sc_write:= true.B
  }

  // ******************************
  //        FIELD INTERFACE
  // ******************************
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

  // ******************************
  //          PREV MEMORY
  // ******************************
  val w_wait_wport = Wire(Bool())
  val w_wait_rport = Wire(Bool())

  // ------------------------------
  //             WRITE
  // ------------------------------
  if (!p.readOnly) {
    w_wait_wport := io.b_in.ctrl.get.op.wa & ~io.b_in.ctrl.get.info.zero & ~m_wport.get.io.b_sout.valid

    m_wport.get.io.b_port <> io.b_port.write
    if (p.useFieldSlct) m_wport.get.io.i_slct.get := w_slct_in
    if (p.useAmo) {
      m_wport.get.io.b_sout.ready := io.b_in.valid & io.b_in.ctrl.get.op.wa & ~io.b_in.ctrl.get.info.zero & ~w_wait_reg & ~w_wait_next & (~io.b_in.ctrl.get.op.a | ~r_rport(w_slct_in.field) | io.b_port.read.ready(w_slct_in.field))
    } else {
      m_wport.get.io.b_sout.ready := io.b_in.valid & io.b_in.ctrl.get.op.wa & ~io.b_in.ctrl.get.info.zero & ~w_wait_reg & ~w_wait_next
    }    
  } else {
    w_wait_wport := false.B
    for (fs <- 0 until p.nFieldSlct) {
      io.b_port.write.ready(fs) := true.B
    }
  }

  // ------------------------------
  //             READ
  // ------------------------------
  if (p.useAmo) {
    w_wait_rport := (io.b_in.ctrl.get.op.ra | w_sc_valid) & ~io.b_port.read.ready(w_slct_in.field) & r_rport(w_slct_in.field)
    io.b_port.read.valid := io.b_in.valid & (io.b_in.ctrl.get.op.ra | w_sc_valid) & r_rport(w_slct_in.field)
  } else {
    w_wait_rport := io.b_in.ctrl.get.op.ra & ~io.b_port.read.ready(w_slct_in.field)
    io.b_port.read.valid := io.b_in.valid & io.b_in.ctrl.get.op.ra
  }
  if (p.useField) io.b_port.read.field.get := io.b_in.field.get
  io.b_port.read.data := Mux(w_sc_valid, ~w_sc_rsv, io.b_in.data.get.asUInt)

  if (p.useAmo) {
    when (io.b_in.valid & (w_sc_valid | io.b_in.ctrl.get.op.a)) {
      when (r_rport(w_slct_in.field)) {
        r_rport(w_slct_in.field) := ~io.b_port.read.ready(w_slct_in.field) | m_wport.get.io.b_sout.valid
      }.otherwise {
        r_rport(w_slct_in.field) := m_wport.get.io.b_sout.valid
      }
    }
  }

  // ------------------------------
  //             WAIT
  // ------------------------------
  w_wait_port := io.b_in.valid & (w_wait_rport | w_wait_wport)
  
  // ******************************
  //            DEPEND
  // ******************************
  if (p.useAckReg && !p.readOnly) {
    for (fs <- 0 until p.nFieldSlct) {
      io.o_dep.get(fs).valid := m_reg.get.io.o_val.valid(fs)
      io.o_dep.get(fs).lock := (fs.U =/= w_slct_out.field) | ~io.b_out.get.ready
      io.o_dep.get(fs).hart := m_reg.get.io.o_val.ctrl.get(fs).info.hart
      io.o_dep.get(fs).rw := m_reg.get.io.o_val.ctrl.get(fs).op.wa
      io.o_dep.get(fs).addr := m_reg.get.io.o_val.ctrl.get(fs).addr
      if (p.usePftch) io.o_dep.get(fs).pftch.get := m_reg.get.io.o_val.ctrl.get(fs).pftch.get    
      if (p.useFieldSlct) {
        io.o_dep.get(fs).field.get := fs.U
      } else if (p.useField) {
        io.o_dep.get(0).field.get := m_reg.get.io.o_val.field.get
      }  
    }
  }

  // ******************************
  //         NEXT CONTROLLER
  // ******************************
  w_wait_next := false.B

  if (!p.readOnly) {
    val m_demux = Module(new GenSDemux(p, UInt(0.W), UInt((p.nDataByte * 8).W)))
    val m_next = Module(new GenDFifo(p, UInt(0.W), UInt((p.nDataByte * 8).W), 2, p.nWriteFifoDepth, 1, 1))
    
    w_wait_next := ~m_demux.io.b_sin.ready

    if (p.useFieldSlct) m_demux.io.i_slct.get := w_slct_in
    m_demux.io.b_sin.valid := io.b_in.valid & io.b_in.ctrl.get.op.wa & w_sc_write & ~w_wait_port & ~w_wait_reg
    if (p.useField) m_demux.io.b_sin.field.get := io.b_in.field.get
    m_demux.io.b_sin.data.get := m_wport.get.io.b_sout.data.get

    for (fs <- 0 until p.nFieldSlct) {
      m_next.io.i_flush(fs) := false.B
    }
    m_next.io.b_din(0) <> m_demux.io.b_dout
    m_next.io.b_dout(0) <> io.b_next.get
  }

  // ******************************
  //           REGISTERS
  // ******************************
  io.b_in.ready := ~(w_wait_port | w_wait_next | w_wait_reg)

  if (!p.readOnly) {
    if (p.useAckReg) {
      w_wait_reg := ~m_reg.get.io.b_sin.ready

      for (fs <- 0 until p.nFieldSlct) {
        m_reg.get.io.i_flush(fs) := false.B
      }
  
      if (p.useFieldSlct) {
        m_reg.get.io.i_slct_in.get := w_slct_in
        m_reg.get.io.i_slct_out.get := w_slct_out
      }
  
      m_reg.get.io.b_sin.valid := io.b_in.valid & io.b_in.ctrl.get.op.wa & ~w_wait_port & ~w_wait_next & w_sc_write
      if (p.useField) m_reg.get.io.b_sin.field.get := io.b_in.field.get
      m_reg.get.io.b_sin.ctrl.get := io.b_in.ctrl.get
      m_reg.get.io.b_sin.data.get.sreg := Mux(io.b_in.ctrl.get.info.zero, 0.U, m_wport.get.io.b_sout.data.get)
      if (p.useAmo) m_reg.get.io.b_sin.data.get.smem := io.b_in.data.get.asUInt else m_reg.get.io.b_sin.data.get.smem := DontCare
  
      io.b_out.get <> m_reg.get.io.b_sout
  
    } else {
      w_wait_reg := ~io.b_out.get.ready
  
      io.b_out.get.valid := io.b_in.valid & io.b_in.ctrl.get.op.wa & ~w_wait_port & ~w_wait_next & w_sc_write
      if (p.useField) io.b_out.get.field.get := io.b_in.field.get
      io.b_out.get.ctrl.get := io.b_in.ctrl.get
      io.b_out.get.data.get.sreg := Mux(io.b_in.ctrl.get.info.zero, 0.U, m_wport.get.io.b_sout.data.get)
      if (p.useAmo) io.b_out.get.data.get.smem := io.b_in.data.get.asUInt else io.b_out.get.data.get.smem := DontCare
    }
  } else {
    w_wait_reg := false.B
  } 
  
  // ******************************
  //             PEND
  // ******************************
  if (!p.readOnly && p.useAckReg) {
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
  //            CBO
  // ******************************
  for (c <- 0 until p.nCbo) {
    io.b_cbo(c).ready := io.b_cbo(c).valid

    if (!p.readOnly && p.useAckReg) {
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
    if (!p.readOnly && p.useAckReg) {
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
    dontTouch(io.b_in.ctrl.get.mtd.get)
  }
}

object AckStage extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new AckStage(PrevUnitConfigBase), args)
}
