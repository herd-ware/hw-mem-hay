/*
 * File: zero.scala                                                            *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-02 01:43:10 pm                                       *
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
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useField, p.nField, p.nTagBit, p.nSet)))

    val b_port = Vec(p.nPrevPort, Vec(p.nFieldTag, new ZeroIO(p)))
  })

  val init_entry = Wire(Vec(p.nFieldTag, new ZeroEntryBus(p)))

  for (f <- 0 until p.nFieldTag) {
    init_entry(f) := DontCare
    init_entry(f).valid := false.B
  }

  val r_entry = RegInit(init_entry) 

  // ******************************
  //        FIELD INTERFACE
  // ******************************
  val w_field_cbo = Wire(Vec(p.nCbo, UInt(log2Ceil(p.nFieldTag).W)))

  for (c <- 0 until p.nCbo) {
    if (p.useField) {
      w_field_cbo(c) := io.b_cbo(c).field.get
    } else {
      w_field_cbo(c) := 0.U
    }
  }

  // ******************************
  //              CBO
  // ******************************
  val w_cbo_ready = Wire(Vec(p.nFieldTag, Vec(p.nCbo + 1, Bool())))

  for (f <- 0 until p.nFieldSlct) {
    w_cbo_ready(f)(0) := ~r_entry(f).valid 
  }

  for (c <- 0 until p.nCbo) {
    io.b_cbo(c).ready := false.B
  }

  if (!p.readOnly) {
    for (c <- 0 until p.nCbo) {
      for (f <- 0 until p.nFieldTag) {
        when (f.U === w_field_cbo(c)) {
          w_cbo_ready(f)(c + 1) := w_cbo_ready(f)(c) & ~(io.b_cbo(c).valid | io.b_cbo(c).zero)

          when (w_cbo_ready(f)(c) & io.b_cbo(c).valid & io.b_cbo(c).zero) {
            io.b_cbo(c).ready := true.B

            r_entry(f).valid := true.B
            r_entry(f).hart := io.b_cbo(c).hart
            r_entry(f).addr.tag := io.b_cbo(c).tag
            r_entry(f).addr.set := io.b_cbo(c).set
            r_entry(f).addr.data := 0.U
          }
        }.otherwise {
          w_cbo_ready(f)(c + 1) := w_cbo_ready(f)(c)
        }
      }
    }
  }

  // ******************************
  //             PORT
  // ******************************
  val w_port_ready = Wire(Vec(p.nFieldTag, Vec(p.nPrevPort + 1, Bool())))

  for (f <- 0 until p.nFieldTag) {
    w_port_ready(f)(0) := false.B

    for (pp <- 0 until p.nPrevPort) {
      w_port_ready(f)(pp + 1) := w_port_ready(f)(pp) | io.b_port(pp)(f).ready
      io.b_port(pp)(f).valid := ~w_port_ready(f)(pp) & r_entry(f).valid
      io.b_port(pp)(f).hart := r_entry(f).hart
      io.b_port(pp)(f).addr := r_entry(f).addr
    }
  }

  // ******************************
  //             UPDATE
  // ******************************
  for (f <- 0 until p.nFieldTag) {
    when (r_entry(f).valid & w_port_ready(f)(p.nPrevPort)) {
      when (r_entry(f).addr.data === (p.nData - 1).U) {
        r_entry(f).valid := false.B
      }.otherwise {
        r_entry(f).addr.data := r_entry(f).addr.data + 1.U
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