/*
 * File: op.scala                                                              *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:17 pm                                       *
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


class OpStage(p: NextParams) extends Module {
  val io = IO(new Bundle {
    val b_dome = if (p.useDome) Some(Vec(p.nDome, new DomeIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useDome, p.nDome, p.nTagBit, p.nSet)))

    val i_slct_in = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val b_in = Flipped(new GenSRVIO(p, new NextOpCtrlBus(p), UInt(0.W)))

    val b_prev = if (!p.readOnly) Some(Vec(p.nPrevPort, Flipped(new GenDRVIO(p, UInt(0.W), UInt((p.nDataByte * 8).W))))) else None

    val b_port = new Mb4sAckIO(p.pNextBus)

    val b_rep = new CacheWriteIO(p)
    val o_end = Output(new CachePendBus(p))
  })

  // ******************************
  //            STATUS
  // ******************************
  val w_wait = Wire(Bool())

  val w_wait_rep = Wire(Bool())
  val w_wait_rport = Wire(Bool())
  val w_wait_cache = Wire(Bool())

  val w_wait_prev = Wire(Bool())
  val w_wait_wport = Wire(Bool())

  // ******************************
  //         DOME INTERFACE
  // ******************************
  val w_slct_in = Wire(new SlctBus(p.nDome, p.nPart, 1))

  if (p.useDomeSlct) {
    w_slct_in := io.i_slct_in.get  
  } else {
    w_slct_in.dome := 0.U
    w_slct_in.next := 0.U
    w_slct_in.step := 0.U
  }

  // ******************************
  //             BUS
  // ******************************   
  val r_data = Reg(Vec(p.nDomeSlct, UInt(log2Ceil(p.nData).W)))
  val w_ndata = Wire(UInt(log2Ceil(p.nData).W))
  val w_caddr = Wire(new AddrBus(p))

  val w_is_rep = Wire(Bool())
  val w_is_wt = Wire(Bool())

  val w_rep_first = Wire(Bool())
  val w_rep_last = Wire(Bool())

  w_caddr := io.b_in.ctrl.get.addr
  w_caddr.data := r_data(w_slct_in.dome)
  w_caddr.offset := 0.U
  
  w_ndata := r_data(w_slct_in.dome)
  r_data(w_slct_in.dome) := w_ndata

  w_is_rep := io.b_in.valid & ~io.b_in.ctrl.get.rw
  w_is_wt := io.b_in.valid & io.b_in.ctrl.get.rw

  w_rep_first := (w_caddr.data === 0.U)
  w_rep_last := (w_caddr.data === (p.nData - 1).U)  

  // Replace status
  if (p.nRepCycle > 1) {
    when (w_rep_first) {
      w_wait_rep := w_is_rep
      when (w_is_rep & ~w_wait_rport & ~w_wait_cache) {
        w_ndata := 1.U
      }
    }.elsewhen (w_rep_last) {
      w_wait_rep := ~w_wait_rport | w_wait_cache
      when (~w_wait_rport & ~w_wait_cache) {
        w_ndata := 0.U
      }
    }.otherwise {
      w_wait_rep := true.B
      when (~w_wait_rport & ~w_wait_cache) {
        w_ndata := w_caddr.data + 1.U
      }
    }
  } else {
    w_wait_rep := false.B
  }
  
  // ******************************
  //        PREV CONTROLLER
  // ******************************
  w_wait_prev := false.B
  if (!p.readOnly) {
    for (pp <- 0 until p.nPrevPort) {
      if (p.useDomeSlct) {
        for (ds <- 0 until p.nDomeSlct) {
          io.b_prev.get(pp).ready(ds) := (pp.U === io.b_in.ctrl.get.prev) & (ds.U === w_slct_in.dome) & w_is_wt & ~w_wait_wport

          when ((pp.U === io.b_in.ctrl.get.prev) & (ds.U === w_slct_in.dome)) {
            w_wait_prev := w_is_wt & ~io.b_prev.get(pp).valid(ds)
          }
        }
      } else {
        io.b_prev.get(pp).ready(0) := (pp.U === io.b_in.ctrl.get.prev) & w_is_wt & ~w_wait_wport

        when (pp.U === io.b_in.ctrl.get.prev) {
          w_wait_prev := w_is_wt & ~io.b_prev.get(pp).valid(0)
        }
      }
    }
  }

  // ******************************
  //        GLOBAL STATUS
  // ****************************** 
  w_wait := (w_wait_rep & ~w_rep_last) | w_wait_rport | w_wait_cache | w_wait_prev | w_wait_wport

  io.b_in.ready := ~w_wait

  // ******************************
  //           NEXT MEMORY
  // ******************************
  // ------------------------------
  //             WRITE
  // ------------------------------
  if (!p.readOnly) {
    w_wait_wport := w_is_wt & ~io.b_port.write.ready(w_slct_in.dome)  

    io.b_port.write.valid := w_is_wt & ~w_wait_prev
    if (p.useDome) io.b_port.write.dome.get := io.b_in.dome.get
    io.b_port.write.data := io.b_prev.get(io.b_in.ctrl.get.prev).data.get(w_slct_in.dome)
  } else {
    w_wait_wport := false.B

    io.b_port.write := DontCare
    io.b_port.write.valid := false.B
  }  

  // ------------------------------
  //             READ
  // ------------------------------
  val m_rport = Module(new Mb4sDataSReg(p.pNextBus))

  m_rport.io.b_port <> io.b_port.read

  if (p.useDomeSlct) m_rport.io.i_slct.get := w_slct_in
  m_rport.io.b_sout.ready := w_is_rep & ~w_wait_cache

  w_wait_rport := w_is_rep & ~m_rport.io.b_sout.valid

  // ******************************
  //            REPLACE
  // ******************************
  // Wdata format
  val m_wdata = Module(new BitToByte(p.nNextDataByte, p.nDataByte))
  m_wdata.io.i_in := m_rport.io.b_sout.data.get

  // Connect to port
  io.b_rep.valid := w_is_rep & ~w_wait_rport
  if (p.useDome) io.b_rep.dome.get := io.b_in.dome.get
  io.b_rep.mask := SIZE.toMask(p.nNextDataByte, SIZE.toSize(p.nNextDataByte).U)
  io.b_rep.offset := w_caddr.offset
  io.b_rep.data := w_caddr.memdata()
  io.b_rep.mem := w_caddr.mem()
  io.b_rep.wdata := m_wdata.io.o_out    

  // Wait cache
  w_wait_cache := w_is_rep & ~io.b_rep.ready    
  
  // End replace
  io.o_end.valid := w_is_rep & w_rep_last & ~w_wait_rport & ~w_wait_cache
  io.o_end.set := w_caddr.set
  io.o_end.line := w_caddr.line

  // ******************************
  //             CBO
  // ******************************
  for (f <- 0 until p.nCbo) {
    io.b_cbo(f).ready := true.B 
  }

  // ******************************
  //             DOME
  // ******************************
  if (p.useDome) {
    for (d <- 0 until p.nDome) {
      io.b_dome.get(d).free := true.B 
    }
  } 

  // ******************************
  //            DEBUG
  // ******************************
  if (p.debug) {
    // ------------------------------
    //            SIGNALS
    // ------------------------------
    dontTouch(io.b_in)
    dontTouch(io.b_port)
    dontTouch(io.b_rep) 

    // ------------------------------
    //         DATA FOOTPRINT
    // ------------------------------

    // ------------------------------
    //         MEMORY TRACKER
    // ------------------------------
  }
}

object OpStage extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new OpStage(NextConfigBase), args)
}