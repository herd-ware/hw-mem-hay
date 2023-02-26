/*
 * File: reserve.scala                                                         *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:41:10 pm                                       *
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
import herd.common.mem.mb4s._
import herd.common.tools._


class Reserve(p: PrevParams) extends Module {
  val io = IO(new Bundle {
    val b_port = Vec(p.nPrevPort, new ReserveIO(p.nHart, p.nAddrBit))
  })

  // ******************************
  //           REGISTERS
  // ******************************
  val init_entry = Wire(Vec(p.nHart, new ReserveEntryBus(p)))

  for (h <- 0 until p.nHart) {
    init_entry(h) := DontCare
    init_entry(h).valid := false.B
  }

  val r_entry = RegInit(init_entry) 

  // ******************************
  //            ALLOCATE
  // ******************************
  for (h <- 0 until p.nHart) {
    for (pp <- 0 until p.nPrevPort) {
      when (io.b_port(pp).valid & io.b_port(pp).rsv & (h.U === io.b_port(pp).hart)) {
        r_entry(h).valid := true.B
        r_entry(h).size := io.b_port(pp).size
        r_entry(h).addr := io.b_port(pp).addr
      }
    }
  }

  // ******************************
  //             WRITE
  // ******************************
  for (h <- 0 until p.nHart) {
    for (pp <- 0 until p.nPrevPort) {
      when (io.b_port(pp).valid & io.b_port(pp).rw) {
        when (((io.b_port(pp).size === SIZE.B8.U) | (r_entry(h).size === SIZE.B8.U)) & (io.b_port(pp).addr(p.nAddrBit - 1, 3) === r_entry(h).addr(p.nAddrBit - 1, 3))) {
          r_entry(h).valid := false.B
        }

        when ((io.b_port(pp).addr(p.nAddrBit - 1, 2) === r_entry(h).addr(p.nAddrBit - 1, 2))) {
          r_entry(h).valid := false.B
        }
      }
    }
  }

  // ******************************
  //           RESERVATION
  // ******************************
  for (pp <- 0 until p.nPrevPort) {
    io.b_port(pp).ready := false.B
    for (h <- 0 until p.nHart) {
      when (r_entry(h).valid & (io.b_port(pp).size === r_entry(h).size) & (h.U === io.b_port(pp).hart)) {
        switch (r_entry(h).size) {
          is (SIZE.B4.U)  {io.b_port(pp).ready := (io.b_port(pp).addr(p.nAddrBit - 1, 2) === r_entry(h).addr(p.nAddrBit - 1, 2))}
          is (SIZE.B8.U)  {io.b_port(pp).ready := (io.b_port(pp).addr(p.nAddrBit - 1, 3) === r_entry(h).addr(p.nAddrBit - 1, 3))}
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
    dontTouch(r_entry)

    // ------------------------------
    //         DATA FOOTPRINT
    // ------------------------------

    // ------------------------------
    //         MEMORY TRACKER
    // ------------------------------
  }   
}

object Reserve extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Reserve(PrevConfigBase), args)
}