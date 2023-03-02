/*
 * File: write.scala                                                           *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:41:14 pm                                       *
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


class WriteStage(p: PrevUnitParams) extends Module {
  val io = IO(new Bundle {    
    val i_slct = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_in = Flipped(new GenSRVIO(p, new PrevUnitCtrlBus(p), new WriteDataBus(p)))

    val b_write = new CacheWriteIO(p)
    val b_pftch = if (p.usePftch) Some(new PftchWriteIO(p, p.nPftchEntry)) else None
  })

  val m_alu = if (p.useAmo) Some(Module(new Alu(p.pPrevBus))) else None

  val w_wdata = Wire(UInt(p.nDataBit.W))

  // ******************************
  //            STATUS
  // ******************************
  val w_wait_alu = Wire(Bool())
  val w_wait_write = Wire(Bool())

  // ******************************
  //        FIELD INTERFACE
  // ******************************
  val w_slct_in = Wire(new SlctBus(p.nField, p.nPart, 1))

  if (p.useFieldSlct) {    
    w_slct_in := io.i_slct.get
  } else {
    w_slct_in.field := 0.U
    w_slct_in.next := 0.U
    w_slct_in.step := 0.U
  }  

  // ******************************
  //             DATA
  // ******************************
  if (p.useAmo) {
    w_wait_alu := ~m_alu.get.io.b_req.ready

    m_alu.get.io.b_req.valid := io.b_in.valid & io.b_in.ctrl.get.op.a
    m_alu.get.io.b_req.ctrl.get := io.b_in.ctrl.get.op.amo.get
    m_alu.get.io.b_req.data.get(0) := io.b_in.data.get.sreg
    m_alu.get.io.b_req.data.get(1) := io.b_in.data.get.smem
    m_alu.get.io.b_ack.ready := true.B

    w_wdata := Mux(io.b_in.ctrl.get.op.a, m_alu.get.io.b_ack.data.get, io.b_in.data.get.sreg)
  } else {
    w_wait_alu := false.B
    w_wdata := io.b_in.data.get.sreg
  }

  // ******************************
  //            WRITE
  // ******************************
  val w_wait_cache = Wire(Bool())
  val w_wait_pftch= Wire(Bool())

  if (p.usePftch) {
    w_wait_cache := ~io.b_in.ctrl.get.pftch.get.use & ~io.b_write.ready
    w_wait_pftch := io.b_in.ctrl.get.pftch.get.use & ~io.b_pftch.get.ready
  } else {
    w_wait_cache := ~io.b_write.ready
    w_wait_pftch := false.B
  }

  w_wait_write := io.b_in.valid & (w_wait_cache | w_wait_pftch)

  // ------------------------------
  //             CACHE
  // ------------------------------
  if (p.usePftch) {
    io.b_write.valid := io.b_in.valid & ~w_wait_alu & ~io.b_in.ctrl.get.pftch.get.use 
  } else {
    io.b_write.valid := io.b_in.valid & ~w_wait_alu
  }
  if (p.useFieldSlct) io.b_write.field.get := w_slct_in.field else if (p.useFieldTag) io.b_write.field.get := io.b_in.field.get
  io.b_write.mask := io.b_in.ctrl.get.op.mask
  io.b_write.offset := io.b_in.ctrl.get.addr.offset
  io.b_write.mem := io.b_in.ctrl.get.addr.mem()
  io.b_write.data := io.b_in.ctrl.get.addr.memdata()
  io.b_write.wdata := w_wdata.asTypeOf(io.b_write.wdata)

  // ------------------------------
  //            PREFETCH
  // ------------------------------
  if (p.usePftch) {
    io.b_pftch.get.valid := io.b_in.valid & ~w_wait_alu & io.b_in.ctrl.get.pftch.get.use
    if (p.useFieldSlct) io.b_pftch.get.field.get := w_slct_in.field else if (p.useFieldTag) io.b_pftch.get.field.get := io.b_in.field.get
    io.b_pftch.get.mask := io.b_in.ctrl.get.op.mask
    io.b_pftch.get.entry := io.b_in.ctrl.get.pftch.get.entry
    io.b_pftch.get.data := io.b_in.ctrl.get.addr.data
    io.b_pftch.get.offset := io.b_in.ctrl.get.addr.offset
    io.b_pftch.get.wdata := w_wdata.asTypeOf(io.b_write.wdata)
  } 

  // ******************************
  //             OUTPUT
  // ******************************
  io.b_in.ready := ~(w_wait_alu | w_wait_write)

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

object WriteStage extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new WriteStage(PrevUnitConfigBase), args)
}
