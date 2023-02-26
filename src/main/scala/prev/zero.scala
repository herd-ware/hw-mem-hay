/*
 * File: zero.scala                                                            *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:41:16 pm                                       *
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

import herd.mem.hay.common._ 
import herd.mem.hay.cache._ 


class Zero(p: PrevParams) extends Module {
  val io = IO(new Bundle {
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useDome, p.nDome, p.nTagBit, p.nSet)))

    val b_port = Vec(p.nPrevPort, Vec(p.nDomeTag, new ZeroIO(p)))
  })

  val init_entry = Wire(Vec(p.nDomeTag, new ZeroEntryBus(p)))

  for (d <- 0 until p.nDomeTag) {
    init_entry(d) := DontCare
    init_entry(d).valid := false.B
  }

  val r_entry = RegInit(init_entry) 

  // ******************************
  //         DOME INTERFACE
  // ******************************
  val w_dome_cbo = Wire(Vec(p.nCbo, UInt(log2Ceil(p.nDomeTag).W)))

  for (c <- 0 until p.nCbo) {
    if (p.useDome) {
      w_dome_cbo(c) := io.b_cbo(c).dome.get
    } else {
      w_dome_cbo(c) := 0.U
    }
  }

  // ******************************
  //              CBO
  // ******************************
  val w_cbo_ready = Wire(Vec(p.nDomeTag, Vec(p.nCbo + 1, Bool())))

  for (d <- 0 until p.nDomeSlct) {
    w_cbo_ready(d)(0) := ~r_entry(d).valid 
  }

  for (c <- 0 until p.nCbo) {
    io.b_cbo(c).ready := false.B
  }

  if (!p.readOnly) {
    for (c <- 0 until p.nCbo) {
      for (d <- 0 until p.nDomeTag) {
        when (d.U === w_dome_cbo(c)) {
          w_cbo_ready(d)(c + 1) := w_cbo_ready(d)(c) & ~(io.b_cbo(c).valid | io.b_cbo(c).zero)

          when (w_cbo_ready(d)(c) & io.b_cbo(c).valid & io.b_cbo(c).zero) {
            io.b_cbo(c).ready := true.B

            r_entry(d).valid := true.B
            r_entry(d).hart := io.b_cbo(c).hart
            r_entry(d).addr.tag := io.b_cbo(c).tag
            r_entry(d).addr.set := io.b_cbo(c).set
            r_entry(d).addr.data := 0.U
          }
        }.otherwise {
          w_cbo_ready(d)(c + 1) := w_cbo_ready(d)(c)
        }
      }
    }
  }

  // ******************************
  //             PORT
  // ******************************
  val w_port_ready = Wire(Vec(p.nDomeTag, Vec(p.nPrevPort + 1, Bool())))

  for (d <- 0 until p.nDomeTag) {
    w_port_ready(d)(0) := false.B

    for (pp <- 0 until p.nPrevPort) {
      w_port_ready(d)(pp + 1) := w_port_ready(d)(pp) | io.b_port(pp)(d).ready
      io.b_port(pp)(d).valid := ~w_port_ready(d)(pp) & r_entry(d).valid
      io.b_port(pp)(d).hart := r_entry(d).hart
      io.b_port(pp)(d).addr := r_entry(d).addr
    }
  }

  // ******************************
  //             UPDATE
  // ******************************
  for (d <- 0 until p.nDomeTag) {
    when (r_entry(d).valid & w_port_ready(d)(p.nPrevPort)) {
      when (r_entry(d).addr.data === (p.nData - 1).U) {
        r_entry(d).valid := false.B
      }.otherwise {
        r_entry(d).addr.data := r_entry(d).addr.data + 1.U
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

object Zero extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Zero(PrevConfigBase), args)
}