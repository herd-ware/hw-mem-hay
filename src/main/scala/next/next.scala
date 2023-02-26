/*
 * File: next.scala                                                            *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:12 pm                                       *
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


class Next(p: NextParams) extends Module {
  val io = IO(new Bundle {
    val b_dome = if (p.useDome) Some(Vec(p.nDome, new DomeIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useDome, p.nDome, p.nTagBit, p.nSet)))

    val i_slct_prev = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val i_slct_write = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    
    val b_prev_ctrl = Vec(p.nPrevPort, Flipped(new NextCtrlIO(p, p.nHart)))
    val b_prev_data = if (!p.readOnly) Some(Vec(p.nPrevPort, Flipped(new GenDRVIO(p, UInt(0.W), UInt((p.nDataByte * 8).W))))) else None
    
    val b_port = new Mb4sIO(p.pNextBus)
    
    val b_rep = new CacheWriteIO(p)
    val o_end = Output(new CachePendBus(p))
  })
  
  // ******************************
  //            MODULES
  // ******************************  
  val m_mem = Module(new MemStage(p))
  val m_op = Module(new OpStage(p))

  // ******************************
  //             MEM
  // ******************************
  if (p.useDome) m_mem.io.b_dome.get <> io.b_dome.get
  m_mem.io.b_cbo <> io.b_cbo

  if (p.useDomeSlct) {
    m_mem.io.i_slct_prev.get := io.i_slct_prev.get
    m_mem.io.i_slct_out.get := io.i_slct_write.get
  }

  m_mem.io.b_prev <> io.b_prev_ctrl
  m_mem.io.b_port <> io.b_port.req

  // ******************************
  //              OP
  // ******************************
  if (p.useDome) m_op.io.b_dome.get <> io.b_dome.get
  m_op.io.b_cbo <> io.b_cbo
  
  if (p.useDomeSlct) m_op.io.i_slct_in.get := io.i_slct_write.get
  m_op.io.b_in <> m_mem.io.b_out
  m_op.io.b_port.read <> io.b_port.read
  m_op.io.b_port.write <> io.b_port.write
  if (!p.readOnly) m_op.io.b_prev.get <> io.b_prev_data.get
  m_op.io.b_rep <> io.b_rep
  io.o_end := m_op.io.o_end

  // ******************************
  //             CBO
  // ******************************
  for (c <- 0 until p.nCbo) {
    io.b_cbo(c).ready := m_mem.io.b_cbo(c).ready & m_op.io.b_cbo(c).ready
  }

  // ******************************
  //             DOME
  // ******************************
  if (p.useDome) {
    for (d <- 0 until p.nDome) {
      io.b_dome.get(d).free := m_mem.io.b_dome.get(d).free & m_op.io.b_dome.get(d).free
    }
  }
}

object Next extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Next(NextConfigBase), args)
}