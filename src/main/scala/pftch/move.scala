/*
 * File: move.scala                                                            *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-02 01:53:28 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay.pftch

import chisel3._
import chisel3.util._

import herd.common.gen._
import herd.common.tools._
import herd.common.field._
import herd.common.mem.mb4s._
import herd.mem.hay.common._
import herd.mem.hay.cache._


class Move (p: PftchParams) extends Module {
  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useField, p.nField, p.nTagBit, p.nSet)))

    val i_slct_acc = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_rep = new CacheRepIO(p)

    val b_move = new PftchMoveIO(p)

    val i_slct_read = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_read = new PftchReadIO(p, p.nPftchEntry)

    val i_slct_write = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_write = new CacheWriteIO(p)
    val o_end_pftch = Output(new PftchPendBus(p))
    val o_end_cache = Output(new CachePendBus(p))
  })  

  // ******************************
  //           REGISTERS
  // ******************************
  val m_read = Module(new GenSReg(p, new PftchCtrlBus(p), UInt(0.W), false, false, false))
  val m_write = Module(new GenSReg(p, new PftchCtrlBus(p), UInt(0.W), false, false, true))
  
  // ******************************
  //            STATUS
  // ******************************
  val w_flush = Wire(Bool())
  val w_wait_read = Wire(Bool())
  val w_wait_write = Wire(Bool())

  // ******************************
  //        FIELD INTERFACE
  // ******************************  
  val w_slct_acc = Wire(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_read = Wire(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_write = Wire(new SlctBus(p.nField, p.nPart, 1))

  if (p.useFieldSlct) {
    w_slct_acc := io.i_slct_acc.get
    w_slct_read := io.i_slct_read.get
    w_slct_write := io.i_slct_write.get
  } else {
    w_slct_acc.field := 0.U
    w_slct_acc.next := 0.U
    w_slct_acc.step := 0.U
    w_slct_read := w_slct_acc
    w_slct_write := w_slct_acc
  }

  // ******************************
  //             MOVE
  // ******************************  
  val w_move = Wire(new GenSVBus(p, new PftchCtrlBus(p), UInt(0.W)))
  
  // ------------------------------
  //    PFTCH CTRL & CACHE CHECK
  // ------------------------------
  w_move.valid := io.b_move.ready & io.b_rep.ready & io.b_rep.alloc & ~w_flush
  if (p.useFieldSlct) w_move.field.get := w_slct_acc.field else if (p.useFieldTag) w_move.field.get := io.b_move.field.get
  w_move.ctrl.get.hart := 0.U
  w_move.ctrl.get.addr.fromFull(Cat(io.b_move.tag, 0.U(log2Ceil(p.nLineByte).W)))
  w_move.ctrl.get.addr.line := io.b_rep.line
  w_move.ctrl.get.entry := io.b_move.entry

  io.b_rep.valid := io.b_move.ready & ~w_wait_read & ~w_flush
  if (p.useField) io.b_rep.field.get := w_move.field.get
  io.b_rep.check := false.B
  io.b_rep.empty := ~io.b_move.used
  io.b_rep.tag := w_move.ctrl.get.addr.tag
  io.b_rep.set := w_move.ctrl.get.addr.set

  io.b_move.valid := io.b_rep.ready & io.b_rep.alloc & ~w_wait_read
  if (p.useFieldSlct) io.b_move.field.get := w_slct_acc.field

  // ------------------------------
  //           REGISTERS
  // ------------------------------
  if (p.useFieldSlct) {
    m_read.io.i_slct_in.get := w_slct_acc
    m_read.io.i_slct_out.get := w_slct_read
  }

  for (fs <- 0 until p.nFieldSlct) {
    m_read.io.i_flush(fs) := false.B
  }

  w_wait_read := ~m_read.io.b_sin.ready

  m_read.io.b_sin.valid := w_move.valid & ~w_flush
  if (p.useField) m_read.io.b_sin.field.get := w_move.field.get
  m_read.io.b_sin.ctrl.get.hart := w_move.ctrl.get.hart  
  m_read.io.b_sin.ctrl.get.entry := w_move.ctrl.get.entry
  m_read.io.b_sin.ctrl.get.addr := w_move.ctrl.get.addr
  m_read.io.b_sin.ctrl.get.addr.data := 0.U


  // ******************************
  //             READ
  // ******************************  
  val r_read_data = RegInit(VecInit(Seq.fill(p.nFieldSlct)(0.U(log2Ceil(p.nData).W))))
  val w_read_cdata = Wire(UInt(log2Ceil(p.nData).W))
  val w_read_ndata = Wire(UInt(log2Ceil(p.nData).W))

  w_read_cdata := r_read_data(w_slct_read.field)
  w_read_ndata := r_read_data(w_slct_read.field)
  r_read_data(w_slct_read.field) := w_read_ndata

  val w_read = Wire(new GenSVBus(p, new PftchCtrlBus(p), UInt(0.W)))  

  w_read.valid := m_read.io.b_sout.valid
  if (p.useField) w_read.field.get := m_read.io.b_sout.field.get
  w_read.ctrl.get := m_read.io.b_sout.ctrl.get
  w_read.ctrl.get.addr.data := w_read_cdata

  // ------------------------------
  //              FSM
  // ------------------------------
  m_read.io.b_sout.ready := ~w_wait_write & io.b_read.ready & (w_read_cdata === (p.nData - 1).U)

  when (w_read.valid & ~w_wait_write & io.b_read.ready) {
    when (w_read_cdata === (p.nData - 1).U) {
      w_read_ndata := 0.U
    }.otherwise {
      w_read_ndata := w_read_cdata + 1.U
    }
  }

  // ------------------------------
  //            MEMORY
  // ------------------------------
  io.b_read.valid := w_read.valid & ~w_wait_write
  if (p.useField) io.b_read.field.get := w_read.field.get
  io.b_read.mask := SIZE.toMask(p.nNextDataByte, SIZE.toSize(p.nNextDataByte).U)
  io.b_read.entry := w_read.ctrl.get.entry
  io.b_read.data := w_read.ctrl.get.addr.data
  io.b_read.offset := w_read.ctrl.get.addr.offset
  
  // ------------------------------
  //           REGISTERS
  // ------------------------------
  if (p.useFieldSlct) {
    m_write.io.i_slct_in.get := w_slct_read
    m_write.io.i_slct_out.get := w_slct_write
  }

  for (fs <- 0 until p.nFieldSlct) {
    m_write.io.i_flush(fs) := false.B
  }

  w_wait_write := ~m_write.io.b_sin.ready

  m_write.io.b_sin.valid := w_read.valid & io.b_read.ready
  if (p.useField) m_write.io.b_sin.field.get := w_read.field.get
  m_write.io.b_sin.ctrl.get.hart := w_read.ctrl.get.hart  
  m_write.io.b_sin.ctrl.get.entry := w_read.ctrl.get.entry
  m_write.io.b_sin.ctrl.get.addr := w_read.ctrl.get.addr

  // ******************************
  //             WRITE
  // ******************************  
  val w_write = Wire(new GenSVBus(p, new PftchCtrlBus(p), UInt(0.W)))  

  w_write := DontCare
  w_write.valid := m_write.io.b_sout.valid
  if (p.useField) w_write.field.get := m_write.io.b_sout.field.get
  w_write.ctrl.get := m_write.io.b_sout.ctrl.get

  // ------------------------------
  //          READ DATA
  // ------------------------------
  val r_rdata_av = RegInit(VecInit(Seq.fill(p.nFieldSlct)(false.B)))
  val r_rdata = Reg(Vec(p.nFieldSlct, Vec(p.nDataByte, UInt(8.W))))

  // Local field interface
  val r_slct_rdata = Reg(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_rdata = Wire(new SlctBus(p.nField, p.nPart, 1))

  if (p.useFieldSlct) {
    r_slct_rdata := w_slct_read
    w_slct_rdata := r_slct_rdata
  } else {
    w_slct_rdata := w_slct_acc
  }
  
  // Back up rdata
  when (~r_rdata_av(w_slct_rdata.field)) {
    r_rdata_av(w_slct_rdata.field) := m_write.io.o_val.valid(w_slct_rdata.field) & ((w_slct_rdata.field =/= w_slct_write.field) | ~io.b_write.ready)
    r_rdata(w_slct_rdata.field) := io.b_read.rdata
  }

  when(r_rdata_av(w_slct_write.field)) {
    r_rdata_av(w_slct_write.field) := ~io.b_write.ready
  }

  // rdata to use
  val w_rdata = Wire(Vec(p.nDataByte, UInt(8.W)))
  w_rdata := Mux(r_rdata_av(w_slct_write.field), r_rdata(w_slct_write.field), io.b_read.rdata)

  // ------------------------------
  //             CACHE
  // ------------------------------
  m_write.io.b_sout.ready := io.b_write.ready

  io.b_write.valid := w_write.valid
  if (p.useField) io.b_write.field.get := w_write.field.get
  io.b_write.mask := SIZE.toMask(p.nNextDataByte, SIZE.toSize(p.nNextDataByte).U)
  io.b_write.offset := w_write.ctrl.get.addr.offset
  io.b_write.data := w_write.ctrl.get.addr.memdata()
  io.b_write.mem := w_write.ctrl.get.addr.mem()
  io.b_write.wdata := w_rdata
  
  // ------------------------------
  //             END
  // ------------------------------
  io.o_end_pftch.valid := w_write.valid & io.b_write.ready & (w_write.ctrl.get.addr.data === (p.nData - 1).U)
  io.o_end_pftch.entry := w_write.ctrl.get.entry

  io.o_end_cache.valid := w_write.valid & io.b_write.ready & (w_write.ctrl.get.addr.data === (p.nData - 1).U)
  io.o_end_cache.set := w_write.ctrl.get.addr.set
  io.o_end_cache.line := w_write.ctrl.get.addr.line

  // ******************************
  //              CBO
  // ******************************
  for (c <- 0 until p.nCbo) {
    if (p.useFieldSlct) {
      io.b_cbo(c).ready := io.b_cbo(c).valid
      for (fs <- 0 until p.nFieldSlct) {
        when (io.b_cbo(c).inv & (fs.U === io.b_cbo(c).field.get)) {
          io.b_cbo(c).ready := ~m_read.io.o_val.valid(fs) & ~m_write.io.o_val.valid(fs)
        }
      }
    } else if (p.useFieldTag) {
      val w_read_ready = ~m_read.io.o_val.valid(0) | (m_read.io.o_val.field.get =/= io.b_cbo(c).field.get)
      val w_write_ready = ~m_write.io.o_val.valid(0) | (m_write.io.o_val.field.get =/= io.b_cbo(c).field.get)

      io.b_cbo(c).ready := ~io.b_cbo(c).inv | (w_read_ready & w_write_ready)
    } else {
      io.b_cbo(c).ready :=  ~io.b_cbo(c).inv | (~m_read.io.o_val.valid(0) & ~m_write.io.o_val.valid(0))
    } 
  }

  // ******************************
  //            FIELD
  // ******************************
  if (p.useField) {
    for (f <- 0 until p.nField) {
      val w_read_ready = Wire(Bool())
      val w_write_ready = Wire(Bool())

      if (p.useFieldSlct) {
        w_read_ready := ~m_read.io.o_val.valid(f)
        w_write_ready := ~m_write.io.o_val.valid(f)
      } else {
        w_read_ready := ~m_read.io.o_val.valid(0) | (m_read.io.o_val.field.get =/= f.U)
        w_write_ready := ~m_write.io.o_val.valid(0) | (m_write.io.o_val.field.get =/= f.U)
      }      

      io.b_field.get(f).free := w_read_ready & w_write_ready
    }
  } 

  // ******************************
  //             FLUSH
  // ******************************  
  w_flush := false.B

  // Cbo
  for (c <- 0 until p.nCbo) {
    when (io.b_cbo(c).valid & io.b_cbo(c).inv) {
      if (p.useFieldSlct) {
        when (io.b_cbo(c).field.get === w_slct_acc.field) {
          w_flush := true.B
        }
      } else if (p.useFieldTag) {
        when (io.b_cbo(c).field.get === w_move.field.get) {
          w_flush := true.B
        }
      } else {
        w_flush := true.B
      } 
    }
  }

  // Field
  if (p.useField) {
    for (f <- 0 until p.nField) {
      if (p.useFieldSlct) {
        when (io.b_field.get(f).flush & (f.U === w_slct_acc.field)) {
          w_flush := true.B
        }
      } else {
        when (io.b_field.get(f).flush & (f.U === w_move.field.get)) {
          w_flush := true.B
        }
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
    dontTouch(m_write.io.b_sin)
    dontTouch(m_write.io.b_sout)
    dontTouch(io.o_end_pftch)
    dontTouch(io.o_end_cache)
    
    // ------------------------------
    //         DATA FOOTPRINT
    // ------------------------------

    // ------------------------------
    //         MEMORY TRACKER
    // ------------------------------
  }  
}

object Move extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Move(PftchConfigBase), args)
}
