/*
 * File: fetch.scala                                                           *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:33 pm                                       *
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
import herd.common.mem.cbo.{OP => CBOOP}
import herd.mem.hay.common._
import herd.mem.hay.cache._


class Fetch (p: PftchParams) extends Module {
  val io = IO(new Bundle {
    val b_dome = if (p.useDome) Some(Vec(p.nDome, new DomeIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useDome, p.nDome, p.nTagBit, p.nSet)))

    val i_miss = Input(Vec(p.nPrevPort, new MissBus(p, p.nHart)))

    val i_slct_acc = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val b_check = new CacheRepIO(p)

    val b_fetch = new PftchFetchIO(p)

    val i_slct_req = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val b_port = new Mb4sIO(p.pNextPort)

    val i_slct_write = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val o_end = Output(new PftchPendBus(p))
    val b_write = new PftchWriteIO(p, p.nPftchEntry)
  })  

  // ******************************
  //           REGISTERS
  // ******************************
  val m_req = Module(new GenSReg(p, new PftchCtrlBus(p), UInt(0.W), false, false, false))
  val m_ack = Module(new GenSFifo(p, new PftchCtrlBus(p), UInt(0.W), 2, p.nPftchFifoDepth, 1, 1))

  val init_fetch = Wire(Vec(p.nDomeSlct, new MissBus(p, p.nHart)))

  for (ds <- 0 until p.nDomeSlct) {
    init_fetch(ds) := DontCare
    init_fetch(ds).valid := false.B
  }

  val r_fetch = RegInit(init_fetch)
  
  // ******************************
  //            STATUS
  // ******************************
  val w_flush = Wire(Bool())
  val w_wait_req = Wire(Bool())
  val w_wait_ack = Wire(Bool())

  // ******************************
  //         DOME INTERFACE
  // ******************************
  val w_slct_miss = Wire(Vec(p.nPrevPort, UInt(log2Ceil(p.nDomeSlct).W)))
  val w_slct_acc = Wire(new SlctBus(p.nDome, p.nPart, 1))
  val w_slct_req = Wire(new SlctBus(p.nDome, p.nPart, 1))
  val w_slct_write = Wire(new SlctBus(p.nDome, p.nPart, 1))

  if (p.useDomeSlct) {
    for (pp <- 0 until p.nPrevPort) {
      w_slct_miss(pp) := io.i_miss(pp).dome.get
    }
    w_slct_acc := io.i_slct_acc.get
    w_slct_req := io.i_slct_req.get
    w_slct_write := io.i_slct_write.get
  } else {
    for (pp <- 0 until p.nPrevPort) {
      w_slct_miss(pp) := 0.U
    }
    w_slct_acc.dome := 0.U
    w_slct_acc.next := 0.U
    w_slct_acc.step := 0.U
    w_slct_req := w_slct_acc
    w_slct_write := w_slct_acc
  }

  // ******************************
  //             FETCH
  // ******************************  
  val w_fetch = Wire(new MissBus(p, p.nHart))
  val w_fetch_av = Wire(Bool())  
  val w_fetch_here = Wire(Bool())  
  val w_fetch_entry = Wire(UInt(log2Ceil(p.nPftchEntry).W))
  val w_addr_next = Wire(UInt(p.nAddrBit.W))

  // ------------------------------
  //           DEFAULT
  // ------------------------------
  r_fetch(w_slct_acc.dome).addr.fromFull(w_addr_next)

  // TODO: Fetch dome entry
  /*if (p.useDome) {
    for (ds <- 0 until p.nDomeSlct) {
      when (~r_fetch(ds).valid & io.b_dome.get(ds).valid) {
        r_fetch(ds).valid := true.B
        r_fetch(ds).addr.fromFull(w_addr_next)
      }
    }
  }*/

  w_fetch := r_fetch(w_slct_acc.dome)
  if (p.useDomeSlct) {
    w_fetch.dome.get := w_slct_acc.dome
  }/* else {
    w_fetch.dome.get := r_fetch(0).dome.get
  }*/

  w_addr_next := w_fetch.addr.toFull

  // ------------------------------
  //           NEW MISS
  // ------------------------------
  for (pp <- 0 until p.nPrevPort) {
    when (io.i_miss(pp).valid) {
      r_fetch(w_slct_miss(pp)).valid := true.B
      r_fetch(w_slct_miss(pp)).addr := io.i_miss(pp).addr
      if (p.useDomeTag) r_fetch(w_slct_miss(pp)).dome.get := io.i_miss(pp).dome.get
    }      
  }

  // ------------------------------
  //             HINT
  // ------------------------------
  for (c <- 0 until p.nCbo) {
    when (io.b_cbo(c).valid & io.b_cbo(c).op === CBOOP.PFTCH) {
      if (p.useDomeSlct) {
        r_fetch(io.b_cbo(c).dome.get).valid := true.B
        r_fetch(io.b_cbo(c).dome.get).addr := Cat(io.b_cbo(c).tag, io.b_cbo(c).set, 0.U(log2Ceil(p.nLineByte).W))
      } else {
        r_fetch(0).valid := true.B
        r_fetch(0).addr := Cat(io.b_cbo(c).tag, io.b_cbo(c).set, 0.U(log2Ceil(p.nLineByte).W))
        if (p.useDomeTag) r_fetch(0).dome.get := io.b_cbo(c).dome.get
      }
    }
  }

  // ------------------------------
  //            FLUSH
  // ------------------------------
  for (c <- 0 until p.nCbo) {
    when (io.b_cbo(c).valid & io.b_cbo(c).inv) {
      if (p.useDomeSlct) {
        r_fetch(io.b_cbo(c).dome.get).valid := false.B
      } else if (p.useDomeTag) {
        r_fetch(0).valid := r_fetch(0).valid & (io.b_cbo(c).dome.get =/= r_fetch(0).dome.get)
      } else {
        r_fetch(0).valid := false.B
      }
    }    
  }

  if (p.useDome) {
    for (d <- 0 until p.nDome) {
      when (io.b_dome.get(d).flush) {
        if (p.useDomeSlct) {
          r_fetch(d).valid := false.B
        } else {
          r_fetch(0).valid := r_fetch(0).valid & (d.U =/= r_fetch(0).dome.get)
        }
      }
    }
  }  

  // ------------------------------
  //    PFTCH CTRL & CACHE CHECK
  // ------------------------------
  io.b_check.valid := w_fetch.valid & io.b_fetch.ready & ~w_wait_req
  io.b_check.hart := w_fetch.hart
  if (p.useDome) io.b_check.dome.get := w_fetch.dome.get
  io.b_check.check := true.B
  io.b_check.empty := false.B
  io.b_check.tag := w_fetch.addr.tag
  io.b_check.set := w_fetch.addr.set

  io.b_fetch.valid := w_fetch.valid & io.b_check.ready & ~io.b_check.found & ~w_wait_req & ~w_flush
  if (p.useDome) io.b_fetch.dome.get := w_fetch.dome.get
  io.b_fetch.tag := w_fetch.addr.toLineAddr()  

  w_fetch_av := io.b_fetch.ready
  w_fetch_here := io.b_fetch.here
  w_fetch_entry := io.b_fetch.entry

  // ------------------------------
  //           REGISTERS
  // ------------------------------
  if (p.useDomeSlct) {
    m_req.io.i_slct_in.get := w_slct_acc
    m_req.io.i_slct_out.get := w_slct_req
  }
  
  w_wait_req := ~m_req.io.b_sin.ready

  for (ds <- 0 until p.nDomeSlct) {
    m_req.io.i_flush(ds) := false.B
  }

  m_req.io.b_sin.valid := w_fetch.valid & io.b_check.ready & ~io.b_check.found & io.b_fetch.ready & ~io.b_fetch.here & ~w_flush
  if (p.useDome) m_req.io.b_sin.dome.get := w_fetch.dome.get
  m_req.io.b_sin.ctrl.get.hart := w_fetch.hart  
  m_req.io.b_sin.ctrl.get.entry := w_fetch_entry
  m_req.io.b_sin.ctrl.get.addr := w_fetch.addr
  m_req.io.b_sin.ctrl.get.addr.line := io.b_check.line
  m_req.io.b_sin.ctrl.get.addr.data := 0.U

  when (w_fetch.valid & io.b_check.ready & io.b_fetch.ready & ~w_wait_req) {
    w_addr_next := ((w_fetch.addr.toLineAddr() + 1.U) << log2Ceil(p.nLineByte).U)
  }

  // ******************************
  //              REQ
  // ******************************  
  val r_req_data = RegInit(VecInit(Seq.fill(p.nDomeSlct)(0.U(log2Ceil(p.nData).W))))
  val w_req_cdata = Wire(UInt(log2Ceil(p.nData).W))
  val w_req_ndata = Wire(UInt(log2Ceil(p.nData).W))

  w_req_cdata := r_req_data(w_slct_req.dome)
  w_req_ndata := r_req_data(w_slct_req.dome)
  r_req_data(w_slct_req.dome) := w_req_ndata

  val w_req = Wire(new GenSVBus(p, new PftchCtrlBus(p), UInt(0.W)))  
  
  w_req.valid := m_req.io.b_sout.valid
  if (p.useDome) w_req.dome.get := m_req.io.b_sout.dome.get
  w_req.ctrl.get := m_req.io.b_sout.ctrl.get
  w_req.ctrl.get.addr.data := w_req_cdata

  // ------------------------------
  //              FSM
  // ------------------------------
  m_req.io.b_sout.ready := ~w_wait_ack & io.b_port.req.ready(w_slct_req.dome) & (w_req_cdata === (p.nData - 1).U)

  when (m_req.io.b_sout.valid & ~w_wait_ack & io.b_port.req.ready(w_slct_req.dome)) {
    when (w_req_cdata === (p.nData - 1).U) {
      w_req_ndata := 0.U
    }.otherwise {
      w_req_ndata := w_req_cdata + 1.U
    }
  }

  // ------------------------------
  //          NEXT MEMORY
  // ------------------------------
  io.b_port.req.valid := w_req.valid
  if (p.useDome) io.b_port.req.dome.get := w_req.dome.get
  io.b_port.req.ctrl.hart := w_req.ctrl.get.hart
  io.b_port.req.ctrl.op := OP.R
  io.b_port.req.ctrl.size := SIZE.toSize(p.nNextDataByte).U
  io.b_port.req.ctrl.addr := w_req.ctrl.get.addr.toFull()

  // ------------------------------
  //             FIFO
  // ------------------------------
  if (p.useDomeSlct) {
    m_ack.io.i_slct_in.get(0) := w_slct_req
    m_ack.io.i_slct_out.get := w_slct_write
  }

  w_wait_ack := ~m_ack.io.b_sin(0).ready

  for (ds <- 0 until p.nDomeSlct) {
    m_ack.io.i_flush(ds) := false.B
  }

  m_ack.io.b_sin(0).valid := w_req.valid & io.b_port.req.ready(w_slct_req.dome)
  if (p.useDome) m_ack.io.b_sin(0).dome.get := w_req.dome.get
  m_ack.io.b_sin(0).ctrl.get := w_req.ctrl.get

  // ******************************
  //              ACK
  // ******************************  
  val w_ack = Wire(new GenSVBus(p, new PftchCtrlBus(p), UInt(0.W)))  

  w_ack := DontCare
  w_ack.valid := false.B

  // ------------------------------
  //          NEXT MEMORY
  // ------------------------------
  val m_data = Module(new Mb4sDataSReg(p.pNextPort))

  m_data.io.b_port <> io.b_port.read

  io.b_port.write := DontCare
  io.b_port.write.valid := false.B

  if (p.useDomeSlct) m_data.io.i_slct.get := w_slct_write
  m_data.io.b_sout.ready := m_ack.io.b_sout(0).valid & io.b_write.ready
  m_ack.io.b_sout(0).ready := m_data.io.b_sout.valid & io.b_write.ready

  w_ack.valid := m_ack.io.b_sout(0).valid
  if (p.useDomeTag) w_ack.dome.get := m_ack.io.b_sout(0).dome.get
  w_ack.ctrl.get := m_ack.io.b_sout(0).ctrl.get

  // ------------------------------
  //             WRITE
  // ------------------------------
  val m_wdata = Module(new BitToByte(p.nNextDataByte, p.nDataByte))
  
  m_wdata.io.i_in := m_data.io.b_sout.data.get

  io.b_write.valid := w_ack.valid & m_data.io.b_sout.valid
  if (p.useDome) io.b_write.dome.get := w_ack.dome.get
  io.b_write.mask := SIZE.toMask(p.nNextDataByte, SIZE.toSize(p.nNextDataByte).U)
  io.b_write.entry := w_ack.ctrl.get.entry
  io.b_write.data := w_ack.ctrl.get.addr.data
  io.b_write.offset := w_ack.ctrl.get.addr.offset
  io.b_write.wdata := m_wdata.io.o_out

  // ------------------------------
  //             END
  // ------------------------------
  io.o_end.valid := w_ack.valid & m_data.io.b_sout.valid & io.b_write.ready & (w_ack.ctrl.get.addr.data === (p.nData - 1).U)
  io.o_end.entry := w_ack.ctrl.get.entry

  // ******************************
  //              CBO
  // ******************************
  for (c <- 0 until p.nCbo) {
    if (p.useDomeSlct) {
      io.b_cbo(c).ready := io.b_cbo(c).valid
      for (ds <- 0 until p.nDomeSlct) {
        when (io.b_cbo(c).inv & (ds.U === io.b_cbo(c).dome.get)) {
          io.b_cbo(c).ready := ~m_req.io.o_val.valid(ds) & (m_ack.io.o_pt(ds) === 0.U)
        }
      }
    } else if (p.useDomeTag) {
      val w_req_ready = ~m_req.io.o_val.valid(0) | (m_req.io.o_val.dome.get =/= io.b_cbo(c).dome.get)
      val w_ack_ready = (m_ack.io.o_pt(0) === 0.U) | (m_ack.io.o_val(0).dome.get =/= io.b_cbo(c).dome.get)

      io.b_cbo(c).ready := ~io.b_cbo(c).inv | (w_req_ready & w_ack_ready)
    } else {
      io.b_cbo(c).ready :=  ~io.b_cbo(c).inv | (~m_req.io.o_val.valid(0) & (m_ack.io.o_pt(0) === 0.U))
    } 
  }

  // ******************************
  //             DOME
  // ******************************
  if (p.useDome) {
    for (d <- 0 until p.nDome) {
      val w_req_ready = Wire(Bool())
      val w_ack_ready = Wire(Bool())

      if (p.useDomeSlct) {
        w_req_ready := ~m_req.io.o_val.valid(d)
        w_ack_ready := (m_ack.io.o_pt(d) === 0.U)
      } else {
        w_req_ready := ~m_req.io.o_val.valid(0) | (m_req.io.o_val.dome.get =/= d.U)
        w_ack_ready := (m_ack.io.o_pt(0) === 0.U) | (m_ack.io.o_val(0).dome.get =/= d.U)
      }      

      io.b_dome.get(d).free := w_req_ready & w_ack_ready
    }
  } 

  // ******************************
  //             FLUSH
  // ******************************  
  w_flush := false.B

  // Cbo
  for (c <- 0 until p.nCbo) {
    when (io.b_cbo(c).valid & io.b_cbo(c).inv) {
      if (p.useDomeSlct) {
        when (io.b_cbo(c).dome.get === w_slct_acc.dome) {
          w_flush := true.B
        }
      } else if (p.useDomeTag) {
        when (io.b_cbo(c).dome.get === w_fetch.dome.get) {
          w_flush := true.B
        }
      } else {
        w_flush := true.B
      } 
    }
  }

  // Dome
  if (p.useDome) {
    for (d <- 0 until p.nDome) {
      if (p.useDomeSlct) {
        when (io.b_dome.get(d).flush & (d.U === w_slct_acc.dome)) {
          w_flush := true.B
        }
      } else {
        when (io.b_dome.get(d).flush & (d.U === w_fetch.dome.get)) {
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
    
    // ------------------------------
    //         DATA FOOTPRINT
    // ------------------------------

    // ------------------------------
    //         MEMORY TRACKER
    // ------------------------------
    
  } 
}

object Fetch extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Fetch(PftchConfigBase), args)
}
