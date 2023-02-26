/*
 * File: pftch.scala                                                           *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:25 pm                                       *
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
import herd.common.dome._
import herd.common.mem.mb4s._
import herd.mem.hay.common._
import herd.mem.hay.cache._
import herd.mem.hay.cache.{Mem => PftchMem}


class Pftch (p: PftchParams) extends Module {
  val io = IO(new Bundle {
    val b_dome = if (p.useDome) Some(Vec(p.nDome, new DomeIO(p.nAddrBit, p.nDataBit))) else None
    val b_part = if (p.useDome) Some(new NRsrcIO(1, p.nDome, p.nPart)) else None

    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useDome, p.nDome, p.nTagBit, p.nSet)))

    val b_prev_acc   = Flipped(Vec(p.nPrevPort, new PftchAccIO(p, p.nPftchEntry)))
    val b_prev_read  = Flipped(Vec(p.nPrevPort, new PftchReadIO(p, p.nPftchEntry)))
    val b_prev_write = if (!p.readOnly) Some(Flipped(Vec(p.nPrevPort, new PftchWriteIO(p, p.nPftchEntry)))) else None

    val i_miss = Input(Vec(p.nPrevPort, new MissBus(p, p.nHart)))

    val i_slct_acc = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val b_rep = new CacheRepIO(p)

    val i_slct_req = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val i_slct_read = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val b_port = new Mb4sIO(p.pNextPort)

    val i_slct_write = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val b_write = new CacheWriteIO(p)
    val o_end = Output(new CachePendBus(p))
  })  

  // ******************************
  //            MODULES
  // ******************************  
  val m_mem = Module(new PftchMem(p.pPftchMem))
  val m_ctrl = Module(new Ctrl(p))
  val m_fetch = Module(new Fetch(p))
  val m_move = Module(new Move(p))

  // ******************************
  //              MEM
  // ******************************
  for (pp <- 0 until p.nPrevPort) {
    // Read
    m_mem.io.b_read(pp).valid := io.b_prev_read(pp).valid
    m_mem.io.b_read(pp).mask := io.b_prev_read(pp).mask
    m_mem.io.b_read(pp).addr := Cat(io.b_prev_read(pp).entry, io.b_prev_read(pp).data)
    m_mem.io.b_read(pp).offset := io.b_prev_read(pp).offset
    io.b_prev_read(pp).ready := m_mem.io.b_read(pp).ready
    io.b_prev_read(pp).rdata := m_mem.io.b_read(pp).rdata

    // Write
    if (!p.readOnly) {
      m_mem.io.b_write(pp).valid := io.b_prev_write.get(pp).valid
      m_mem.io.b_write(pp).mask := io.b_prev_write.get(pp).mask
      m_mem.io.b_write(pp).addr := Cat(io.b_prev_write.get(pp).entry, io.b_prev_write.get(pp).data)
      m_mem.io.b_write(pp).offset := io.b_prev_write.get(pp).offset
      m_mem.io.b_write(pp).wdata := io.b_prev_write.get(pp).wdata
      io.b_prev_write.get(pp).ready := m_mem.io.b_write(pp).ready
    }
  }

  // Read
  m_mem.io.b_read(p.nPrevPort).valid := m_move.io.b_read.valid
  m_mem.io.b_read(p.nPrevPort).mask := m_move.io.b_read.mask
  m_mem.io.b_read(p.nPrevPort).addr := Cat(m_move.io.b_read.entry, m_move.io.b_read.data)
  m_mem.io.b_read(p.nPrevPort).offset := m_move.io.b_read.offset
  m_move.io.b_read.rdata := m_mem.io.b_read(p.nPrevPort).rdata
  m_move.io.b_read.ready := m_mem.io.b_read(p.nPrevPort).ready
  
  // Write
  if (!p.readOnly) {
    m_mem.io.b_write(p.nPrevPort).valid := m_fetch.io.b_write.valid
    m_mem.io.b_write(p.nPrevPort).mask := m_fetch.io.b_write.mask
    m_mem.io.b_write(p.nPrevPort).addr := Cat(m_fetch.io.b_write.entry, m_fetch.io.b_write.data)
    m_mem.io.b_write(p.nPrevPort).offset := m_fetch.io.b_write.offset
    m_mem.io.b_write(p.nPrevPort).wdata := m_fetch.io.b_write.wdata
    m_fetch.io.b_write.ready := m_mem.io.b_write(p.nPrevPort).ready
  } else {
    m_mem.io.b_write(0).valid := m_fetch.io.b_write.valid
    m_mem.io.b_write(0).mask := m_fetch.io.b_write.mask
    m_mem.io.b_write(0).addr := Cat(m_fetch.io.b_write.entry, m_fetch.io.b_write.data)
    m_mem.io.b_write(0).offset := m_fetch.io.b_write.offset
    m_mem.io.b_write(0).wdata := m_fetch.io.b_write.wdata
    m_fetch.io.b_write.ready := m_mem.io.b_write(0).ready
  }
  
  // ******************************
  //             CTRL
  // ******************************
  if (p.useDome) {
    m_ctrl.io.b_dome.get <> io.b_dome.get
    m_ctrl.io.b_part.get <> io.b_part.get
  }
  m_ctrl.io.b_cbo <> io.b_cbo

  m_ctrl.io.i_miss := io.i_miss
  m_ctrl.io.b_fetch <> m_fetch.io.b_fetch
  m_ctrl.io.b_move <> m_move.io.b_move
  m_ctrl.io.b_acc <> io.b_prev_acc
  
  m_ctrl.io.i_fetch_end := m_fetch.io.o_end
  m_ctrl.io.i_move_end := m_move.io.o_end_pftch
  for (pp <- 0 until p.nPrevPort) {
    m_ctrl.io.i_use_end(pp).valid := io.b_prev_read(pp).valid & m_mem.io.b_read(pp).ready
    m_ctrl.io.i_use_end(pp).entry := io.b_prev_read(pp).entry
  }
  if (!p.readOnly) {
    for (pp <- 0 until p.nPrevPort) {
      m_ctrl.io.i_use_end(p.nPrevPort + pp).valid := io.b_prev_write.get(pp).valid & m_mem.io.b_write(pp).ready
      m_ctrl.io.i_use_end(p.nPrevPort + pp).entry := io.b_prev_write.get(pp).entry
    }
  }

  // ******************************
  //             FETCH
  // ******************************
  if (p.useDome) m_fetch.io.b_dome.get <> io.b_dome.get
  m_fetch.io.b_cbo <> io.b_cbo

  m_fetch.io.i_miss := io.i_miss
  m_fetch.io.b_port <> io.b_port

  if (p.useDomeSlct) {
    m_fetch.io.i_slct_acc.get := io.i_slct_acc.get
    m_fetch.io.i_slct_req.get := io.i_slct_req.get
    m_fetch.io.i_slct_write.get := io.i_slct_write.get
  }

  // ******************************
  //             MOVE
  // ******************************
  if (p.useDome) m_move.io.b_dome.get <> io.b_dome.get
  m_move.io.b_cbo <> io.b_cbo

  m_move.io.b_write <> io.b_write
  io.o_end := m_move.io.o_end_cache

  if (p.useDomeSlct) {
    m_move.io.i_slct_acc.get := io.i_slct_acc.get
    m_move.io.i_slct_read.get := io.i_slct_read.get
    m_move.io.i_slct_write.get := io.i_slct_write.get
  }

  // ******************************
  //         CACHE REPLACE
  // ******************************
  when (m_fetch.io.b_check.valid) {
    m_fetch.io.b_check <> io.b_rep
    m_move.io.b_rep := DontCare
    m_move.io.b_rep.ready := false.B
  }.otherwise {
    m_move.io.b_rep <> io.b_rep
    m_fetch.io.b_check := DontCare
    m_fetch.io.b_check.ready := false.B
  }

  // ******************************
  //             FLUSH
  // ******************************  
  for (f <- 0 until p.nCbo) {
    m_ctrl.io.b_cbo(f).valid := io.b_cbo(f).valid & m_fetch.io.b_cbo(f).ready & m_move.io.b_cbo(f).ready
    io.b_cbo(f).ready := m_ctrl.io.b_cbo(f).ready & m_fetch.io.b_cbo(f).ready & m_move.io.b_cbo(f).ready
  } 

  // ******************************
  //             DOME
  // ******************************
  if (p.useDome) {
    // ------------------------------
    //           DOME STATE
    // ------------------------------
    for (d <- 0 until p.nDome) {    
      m_ctrl.io.b_dome.get(d).flush := io.b_dome.get(d).flush & m_fetch.io.b_dome.get(d).free & m_move.io.b_dome.get(d).free
      io.b_dome.get(d).free := m_fetch.io.b_dome.get(d).free & m_move.io.b_dome.get(d).free & m_ctrl.io.b_dome.get(d).free
    }

    // ------------------------------
    //           PART STATE
    // ------------------------------
    for (pa <- 0 until p.nPart) {
      val w_dome_flush = io.b_dome.get(io.b_part.get.state(pa).dome).flush
      val w_op_free = m_fetch.io.b_dome.get(io.b_part.get.state(pa).dome).free & m_move.io.b_dome.get(io.b_part.get.state(pa).dome).free

      m_ctrl.io.b_part.get.state(pa).flush := io.b_part.get.state(pa).flush & (~w_dome_flush | w_op_free)
    }    
  }

  // ******************************
  //             DEBUG
  // ******************************
  if (p.debug) {
    
  }  
}

object Pftch extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Pftch(PftchConfigBase), args)
}
