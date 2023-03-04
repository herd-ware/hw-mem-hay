/*
 * File: unit.scala
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-03 02:32:19 pm
 * Modified By: Mathieu Escouteloup
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


class PrevUnit (p: PrevUnitParams) extends Module {
  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useField, p.nField, p.nTagBit, p.nSet)))

    val i_slct = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_port = Flipped(new Mb4sIO(p.pPrevBus))
    val b_zero = if (!p.readOnly) Some(Flipped(Vec(p.nFieldTag, new ZeroIO(p)))) else None

    val b_dep_back = Vec(p.nFieldSlct, new DependStageIO(p))
    val b_dep_bus = Vec(p.nFieldSlct, new DependStageIO(p))
    val b_dep_req = if (p.useReqReg) Some(Vec(p.nFieldSlct, new DependStageIO(p))) else None    
    val b_dep_acc = if (p.useAccReg) Some(Vec(p.nFieldSlct, new DependStageIO(p))) else None
    val i_dep_rw = Input(Vec(p.nFieldSlct, Bool()))
    val b_dep_read = Vec(p.nFieldSlct, new DependStageIO(p))
    val o_dep_ack = if (p.useAckReg && !p.readOnly) Some(Output(Vec(p.nFieldSlct, new DependRegBus(p)))) else None

    val b_cache_acc   = new CacheAccIO(p)
    val b_cache_rep   = new CacheRepIO(p)
    val b_cache_read  = new CacheReadIO(p)
    val b_cache_write = if (!p.readOnly) Some(new CacheWriteIO(p)) else None

    val b_pftch_acc   = if (p.usePftch) Some(new PftchAccIO(p, p.nPftchEntry)) else None
    val b_pftch_read  = if (p.usePftch) Some(new PftchReadIO(p, p.nPftchEntry)) else None
    val b_pftch_write = if (p.usePftch && !p.readOnly) Some(new PftchWriteIO(p, p.nPftchEntry)) else None

    val b_rsv = if (p.useAmo) Some(Flipped(new ReserveIO(p.nHart, p.nAddrBit))) else None

    val b_next_ctrl = new NextCtrlIO(p, p.nHart)
    val b_next_data = if (!p.readOnly) Some(new GenDRVIO(p, UInt(0.W), UInt((p.nDataByte * 8).W))) else None

    val o_miss = Output(new MissBus(p, p.nHart))
    val o_hpc = Output(Vec(p.nHart, new HpcCacheBus()))
    val o_pend = Output(Vec(p.nPendingAcc, new CachePendBus(p)))
  })

  // ******************************
  //            MODULES
  // ******************************  
  val m_req = Module(new ReqStage(p))
  val m_acc = Module(new AccStage(p))
  val m_read = Module(new ReadStage(p))
  val m_ack = Module(new AckStage(p))
  val m_write = if (!p.readOnly) Some(Module(new WriteStage(p))) else None

  // ******************************
  //             REQ
  // ******************************
  if (p.useField) m_req.io.b_field.get <> io.b_field.get
  m_req.io.b_cbo <> io.b_cbo

  if (p.useFieldSlct) m_req.io.i_slct.get := io.i_slct.get
  m_req.io.b_port <> io.b_port.req  
  if (!p.readOnly) m_req.io.b_zero.get <> io.b_zero.get

  m_req.io.b_dep_back <> io.b_dep_back
  m_req.io.b_dep_bus <> io.b_dep_bus
  if (p.useReqReg) m_req.io.b_dep_reg.get <> io.b_dep_req.get

  // ******************************
  //             ACC
  // ******************************
  if (p.useField) m_acc.io.b_field.get <> io.b_field.get
  m_acc.io.b_cbo <> io.b_cbo

  if (p.useFieldSlct) m_acc.io.i_slct.get := m_req.io.o_slct.get
  m_acc.io.b_in <> m_req.io.b_out

  if (p.useAccReg) m_acc.io.b_dep.get := io.b_dep_acc.get  

  m_acc.io.b_acc <> io.b_cache_acc
  m_acc.io.b_rep <> io.b_cache_rep
  if (p.usePftch) m_acc.io.b_pftch.get <> io.b_pftch_acc.get
  if (p.useAmo) m_acc.io.b_rsv.get <> io.b_rsv.get
  m_acc.io.b_next <> io.b_next_ctrl
  io.o_miss := m_acc.io.o_miss
  io.o_hpc := m_acc.io.o_hpc

  // ******************************
  //             READ
  // ******************************
  if (p.useField) m_read.io.b_field.get <> io.b_field.get
  m_read.io.b_cbo <> io.b_cbo

  if (p.useFieldSlct) m_read.io.i_slct.get := m_acc.io.o_slct.get
  m_read.io.b_in <> m_acc.io.b_out

  m_read.io.i_dep_rw <> io.i_dep_rw
  m_read.io.b_dep <> io.b_dep_read

  m_read.io.b_read <> io.b_cache_read
  if (p.usePftch) m_read.io.b_pftch.get <> io.b_pftch_read.get

  // ******************************
  //              ACK
  // ******************************
  if (p.useField) m_ack.io.b_field.get <> io.b_field.get
  m_ack.io.b_cbo <> io.b_cbo

  if (p.useFieldSlct) m_ack.io.i_slct.get := m_read.io.o_slct.get
  m_ack.io.b_in <> m_read.io.b_out  

  m_ack.io.b_port.read  <> io.b_port.read
  m_ack.io.b_port.write <> io.b_port.write
  if (!p.readOnly) m_ack.io.b_next.get <> io.b_next_data.get

  if (p.useAckReg && !p.readOnly) io.o_dep_ack.get := m_ack.io.o_dep.get

  // ******************************
  //             WRITE
  // ******************************
  if (!p.readOnly) {
    if (p.useFieldSlct) m_write.get.io.i_slct.get := m_read.io.o_slct.get
    m_write.get.io.b_in <> m_ack.io.b_out.get  

    if (!p.readOnly) {
      if (p.usePftch) m_write.get.io.b_pftch.get <> io.b_pftch_write.get
      m_write.get.io.b_write <> io.b_cache_write.get
    }
  }

  // ******************************
  //            PENDING
  // ******************************
  var pa: Int = 0
  for (fs <- 0 until p.nFieldSlct) {
    io.o_pend(fs) := m_read.io.o_pend(fs)
  }
  pa = p.nFieldSlct

  if (p.useAccReg) {
    for (fs <- 0 until p.nFieldSlct) {
      io.o_pend(pa + fs) := m_acc.io.o_pend.get(fs)
    }
    pa = pa + p.nFieldSlct
  }

  if (p.useAckReg && !p.readOnly) {
    for (fs <- 0 until p.nFieldSlct) {
      io.o_pend(pa + fs) := m_ack.io.o_pend.get(fs)
    }
    pa = pa + p.nFieldSlct
  }

  // ******************************
  //            FLUSH
  // ******************************
  for (f <- 0 until p.nCbo) {
    io.b_cbo(f).ready := m_req.io.b_cbo(f).ready & m_acc.io.b_cbo(f).ready & m_read.io.b_cbo(f).ready & m_ack.io.b_cbo(f).ready
  }

  // ******************************
  //            FIELD
  // ******************************
  if (p.useField) {
    for (f <- 0 until p.nField) {
      io.b_field.get(f).free := m_req.io.b_field.get(f).free & m_acc.io.b_field.get(f).free & m_read.io.b_field.get(f).free & m_ack.io.b_field.get(f).free
    }
  }
}

object PrevUnit extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new PrevUnit(PrevUnitConfigBase), args)
}
