/*
 * File: ctrl.scala                                                            *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-02 01:40:37 pm                                       *
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
import herd.common.mem.cbo.{OP => CBOOP, BLOCK => CBOBLOCK}
import herd.mem.hay.common._
import herd.mem.hay.cache._


class Ctrl (p: PftchParams) extends Module {
  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(p.nAddrBit, p.nDataBit))) else None
    val b_part = if (p.useField) Some(new NRsrcIO(1, p.nField, p.nPart)) else None

    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useField, p.nField, p.nTagBit, p.nSet)))

    val i_miss = Input(Vec(p.nPrevPort, new MissBus(p, p.nHart)))

    val b_fetch = Flipped(new PftchFetchIO(p))
    val b_move = Flipped(new PftchMoveIO(p))
    val b_acc = Vec(p.nPrevPort, Flipped(new PftchAccIO(p, p.nPftchEntry)))
      
    val i_fetch_end = Input(new PftchPendBus(p))
    val i_move_end = Input(new PftchPendBus(p))
    val i_use_end = Input(Vec(p.nPftchOp, new PftchPendBus(p)))
  })  

  // ******************************
  //           REGISTERS
  // ******************************
  val init_entry = Wire(Vec(p.nPftchEntry, new PftchEntryBus(p)))

  for (pe <- 0 until p.nPftchEntry) {
    init_entry(pe) := DontCare
    init_entry(pe).vtag := false.B
  }

  val r_entry = RegInit(init_entry) 

  // ******************************
  //        RESOURCE STATUS
  // ******************************
  val m_entry = if (p.useField) Some(Module(new Part2Rsrc(1, p.nField, p.nPart, p.nPftchEntry))) else None

  if (p.useField) {
    m_entry.get.io.b_part <> io.b_part.get
  }

  // ******************************
  //          UPDATE STATUS
  // ******************************
  val w_flush = Wire(Vec(p.nPftchEntry, Bool()))
  val w_move = Wire(Vec(p.nPftchEntry, Bool()))
  val w_new_in_use = Wire(Vec(p.nPrevPort + 1, Vec(p.nPftchEntry, UInt(p.nPftchEntryAcc.W))))
  val w_end_in_use = Wire(Vec(p.nPftchOp + 1, Vec(p.nPftchEntry, UInt(p.nPftchEntryAcc.W))))

  // ------------------------------
  //             MOVE
  // ------------------------------
  for (pe <- 0 until p.nPftchEntry) {
    w_move(pe) := r_entry(pe).move
    r_entry(pe).move := w_move(pe)
  }

  // ------------------------------
  //            IN USE
  // ------------------------------  
  // Default
  for (pe <- 0 until p.nPftchEntry) {
    w_new_in_use(0)(pe) := r_entry(pe).in_use

    for (pp <- 1 to p.nPrevPort) {
      w_new_in_use(pp)(pe) := w_new_in_use(pp - 1)(pe)
    }
  }

  // Default
  for (pe <- 0 until p.nPftchEntry) {
    w_end_in_use(0)(pe) := w_new_in_use(p.nPrevPort)(pe)

    for (po <- 1 to p.nPftchOp) {
      w_end_in_use(po)(pe) := w_end_in_use(po - 1)(pe)
    }
  }

  // End read & write
  for (op <- 0 until p.nPftchOp) {
    when (io.i_use_end(op).valid) {
      w_end_in_use(op + 1)(io.i_use_end(op).entry) := (w_end_in_use(op)(io.i_use_end(op).entry) >> 1.U)
    }
  }

  // Register
  for (pe <- 0 until p.nPftchEntry) {
    r_entry(pe).in_use := w_end_in_use(p.nPftchOp)(pe)
  }

  // ------------------------------
  //          END FETCH
  // ------------------------------
  when (io.i_fetch_end.valid) {
    r_entry(io.i_fetch_end.entry).vdata := true.B
  }

  // ------------------------------
  //           END MOVE
  // ------------------------------
  when (io.i_move_end.valid) {
    r_entry(io.i_move_end.entry).vtag := false.B
    r_entry(io.i_move_end.entry).vdata := false.B
  }

  // ------------------------------
  //             MISS
  // ------------------------------  
  for (pp <- 0 until p.nPrevPort) {
    for (pe <- 0 until p.nPftchEntry) {
      if (p.useField) {
        when (io.i_miss(pp).valid & (io.i_miss(pp).field.get === m_entry.get.io.b_rsrc.state(pe).field)) {
          r_entry(pe).old := true.B
        }
      } else {
        when (io.i_miss(pp).valid) {
          r_entry(pe).old := true.B
        }
      }
    }
  }

  // ------------------------------
  //             FLUSH
  // ------------------------------
  for (pe <- 0 until p.nPftchEntry) {
    when (r_entry(pe).vtag & r_entry(pe).vdata & w_flush(pe)) {
      r_entry(pe).vtag := false.B
      r_entry(pe).vdata := false.B
    }
  }

  // ******************************
  //             FETCH
  // ******************************
  val w_fetch_valid = Wire(Vec(p.nPftchEntry, Bool()))
  val w_fetch_empty = Wire(Vec(p.nPftchEntry, Bool()))
  val w_fetch_old = Wire(Vec(p.nPftchEntry, Bool()))
  val w_fetch_here = Wire(Bool())
  val w_fetch_entry = Wire(UInt(log2Ceil(p.nPftchEntry).W))
  
  for (pe <- 0 until p.nPftchEntry) {
    if (p.useField) {
      w_fetch_valid(pe) := ~w_flush(pe) & m_entry.get.io.b_rsrc.state(pe).valid & (io.b_fetch.field.get === m_entry.get.io.b_rsrc.state(pe).field)
    } else {
      w_fetch_valid(pe) := ~w_flush(pe)
    }
    w_fetch_empty(pe) := w_fetch_valid(pe) & ~r_entry(pe).vtag
    w_fetch_old(pe) := w_fetch_valid(pe) & r_entry(pe).vtag & r_entry(pe).vdata & r_entry(pe).old & ~r_entry(pe).used & ~w_new_in_use(p.nPrevPort)(pe).asUInt.orR
  }
  
  w_fetch_here := false.B
  w_fetch_entry := PriorityEncoder(Mux(w_fetch_empty.asUInt.orR, w_fetch_empty.asUInt, w_fetch_old.asUInt))
  for (pe <- 0 until p.nPftchEntry) {
    when (w_fetch_valid(pe) & r_entry(pe).vtag & (io.b_fetch.tag === r_entry(pe).tag)) {
      w_fetch_here := true.B
    }
  }

  io.b_fetch.ready := w_fetch_empty.asUInt.orR | w_fetch_old.asUInt.orR
  io.b_fetch.here := w_fetch_here
  io.b_fetch.entry := w_fetch_entry

  when (io.b_fetch.valid & ~w_fetch_here & (w_fetch_empty.asUInt.orR | w_fetch_old.asUInt.orR)) {
    r_entry(w_fetch_entry).vtag := true.B
    r_entry(w_fetch_entry).vdata := false.B
    r_entry(w_fetch_entry).old := false.B
    r_entry(w_fetch_entry).used := false.B
    r_entry(w_fetch_entry).in_use := 0.U
    r_entry(w_fetch_entry).move := false.B
    r_entry(w_fetch_entry).tag := io.b_fetch.tag
  }

  // ******************************
  //             MOVE
  // ******************************
  val w_move_valid = Wire(Vec(p.nPftchEntry, Bool()))
  val w_move_av = Wire(Vec(p.nPftchEntry, Bool()))
  val w_move_used = Wire(Vec(p.nPftchEntry, Bool()))
  val w_move_entry = Wire(UInt(log2Ceil(p.nPftchEntry).W))

  for (pe <- 0 until p.nPftchEntry) {
    if (p.useFieldSlct) {
      w_move_valid(pe) := ~w_flush(pe) & m_entry.get.io.b_rsrc.state(pe).valid & (io.b_move.field.get === m_entry.get.io.b_rsrc.state(pe).field)
    } else {
      w_move_valid(pe) := ~w_flush(pe)
    }
    w_move_av(pe) := w_move_valid(pe) & r_entry(pe).vtag & r_entry(pe).vdata & ~w_new_in_use(p.nPrevPort)(pe).orR
    w_move_used(pe) := w_move_av(pe) & r_entry(pe).used
  }
  w_move_entry := PriorityEncoder(Mux(w_move_used.asUInt.orR, w_move_used.asUInt, w_move_av.asUInt))

  io.b_move.ready := w_move_av.asUInt.orR
  io.b_move.used := w_move_used.asUInt.orR
  io.b_move.entry := w_move_entry
  io.b_move.tag := r_entry(w_move_entry).tag
  if (p.useFieldTag) io.b_move.field.get := m_entry.get.io.b_rsrc.state(w_move_entry).field

  when (io.b_move.valid) {
    w_move(w_move_entry) := true.B
  }

  // ******************************
  //            ACCESS
  // ******************************
  val w_acc_valid = Wire(Vec(p.nPrevPort, Vec(p.nPftchEntry, Bool())))
  val w_acc_here = Wire(Vec(p.nPrevPort, Bool()))
  val w_acc_av = Wire(Vec(p.nPrevPort, Bool()))
  val w_acc_entry = Wire(Vec(p.nPrevPort, UInt(p.nPftchEntry.W)))

  // Entry status
  for (pp <- 0 until p.nPrevPort) {
    for (pe <- 0 until p.nPftchEntry) {
      if (p.useField) {
        w_acc_valid(pp)(pe) := ~w_flush(pe) & m_entry.get.io.b_rsrc.state(pe).valid & (io.b_acc(pp).field.get === m_entry.get.io.b_rsrc.state(pp).field)
      } else {
        w_acc_valid(pp)(pe) := ~w_flush(pe) 
      } 
    }
  }

  // Verify if needed entry is here
  for (pp <- 0 until p.nPrevPort) {
    w_acc_here(pp) := false.B
    w_acc_av(pp) := false.B
    w_acc_entry(pp) := 0.U

    for (pe <- 0 until p.nPftchEntry) {
      when (r_entry(pe).vtag & ~r_entry(pe).move & (r_entry(pe).tag === Cat(io.b_acc(pp).tag, io.b_acc(pp).set)) & w_acc_valid(pp)(pe)) {
        w_acc_here(pp) := true.B
        w_acc_av(pp) := r_entry(pe).vdata & (io.b_acc(pp).check | ~w_new_in_use(pp)(pe).andR)
        w_acc_entry(pp) := pe.U  
      }
    }
    
    // Ports
    io.b_acc(pp).ready := true.B
    io.b_acc(pp).found := w_acc_here(pp)
    io.b_acc(pp).av := w_acc_av(pp)
    io.b_acc(pp).entry := w_acc_entry(pp)
  }

  // Update registers
  for (pe <- 0 until p.nPftchEntry) {
    for (pp <- 0 until p.nPrevPort) {
      when (io.b_acc(pp).valid & ~io.b_acc(pp).check & w_acc_here(pp) & (pe.U === w_acc_entry(pp))) {
        w_new_in_use(pp + 1)(pe) := (w_new_in_use(pp)(pe) << 1.U) | 1.U
        r_entry(pe).used := true.B
      }
    }
  }

  // ******************************
  //              CBO
  // ******************************
  // ------------------------------
  //            PRESENCE
  // ------------------------------
  val w_cbo_av = Wire(Vec(p.nCbo, Vec(p.nPftchEntry, Bool())))
  val w_cbo_here = Wire(Vec(p.nCbo, Bool()))
  val w_cbo_entry = Wire(Vec(p.nCbo, UInt(p.nPftchEntry.W)))

  for (c <- 0 until p.nCbo) {
    w_cbo_here(c) := false.B
    w_cbo_entry(c) := 0.U

    for (pe <- 0 until p.nPftchEntry) {
      if (p.useField) {
        w_cbo_av(c)(pe) := io.b_cbo(c).valid & m_entry.get.io.b_rsrc.state(pe).valid & (io.b_cbo(c).field.get === m_entry.get.io.b_rsrc.state(pe).field) 
      } else {
        w_cbo_av(c)(pe) := io.b_cbo(c).valid
      }

      when (r_entry(pe).vtag & w_cbo_av(c)(pe) & (r_entry(pe).tag === Cat(io.b_cbo(c).tag, io.b_cbo(c).set))) {
        w_cbo_here(c) := true.B
        w_cbo_entry(c) := pe.U                      
      }
    }
  }

  // ------------------------------
  //             READY
  // ------------------------------
  for (c <- 0 until p.nCbo) {
    io.b_cbo(c).ready := true.B

    when (io.b_cbo(c).valid & io.b_cbo(c).inv) {
      switch (io.b_cbo(c).block) {
        is (CBOBLOCK.LINE) {
          io.b_cbo(c).ready := ~w_cbo_here(c) | ~r_entry(w_cbo_entry(c)).vtag
        }
        is (CBOBLOCK.SET) {
          for (pe <- 0 until p.nPftchEntry) {
            when (w_cbo_av(c)(pe) & r_entry(pe).vtag & (io.b_cbo(c).set === r_entry(pe).tag(log2Ceil(p.nSet) - 1, 0))) {
              io.b_cbo(c).ready := false.B
            }
          }
        }
        is (CBOBLOCK.FULL) {
          for (pe <- 0 until p.nPftchEntry) {
            when (w_cbo_av(c)(pe) & r_entry(pe).vtag) {
              io.b_cbo(c).ready := false.B
            }
          }
        }
      }
    }
  }

  // ******************************
  //            FIELD
  // ******************************
  if (p.useField) {
    for (f <- 0 until p.nField) {
      io.b_field.get(f).free := true.B
    }

    for (pe <- 0 until p.nPftchEntry) {
      m_entry.get.io.b_rsrc.state(pe).free := ~r_entry(pe).vtag & ~r_entry(pe).vdata
    }
  } 

  // ******************************
  //             FLUSH
  // ******************************
  for (pe <- 0 until p.nPftchEntry) {
    if (p.useField) w_flush(pe) := m_entry.get.io.b_rsrc.state(pe).flush else w_flush(pe) := false.B

    for (c <- 0 until p.nCbo) {
      when ((io.b_cbo(c).op === CBOOP.INVAL) | (io.b_cbo(c).op === CBOOP.FLUSH)) {
        when (w_cbo_av(c)(pe) & ((io.b_cbo(c).block === CBOBLOCK.FULL) | ((io.b_cbo(c).block === CBOBLOCK.SET) & (io.b_cbo(c).set === r_entry(pe).tag(log2Ceil(p.nSet) - 1, 0))) | (w_cbo_here(c) & (io.b_cbo(c).block === CBOBLOCK.LINE) & (pe.U === w_cbo_entry(c))))) {        
          w_flush(pe) := true.B
        }
      }
    }
  }
  
  // ******************************
  //             DEBUG
  // ******************************
  val w_acc_tag = Wire(Vec(p.nPrevPort, UInt(p.nPftchTagBit.W)))

  for (pp <- 0 until p.nPrevPort) {
    w_acc_tag(pp) := Cat(io.b_acc(pp).tag, io.b_acc(pp).set)
  }  

  if (p.debug) {
    // ------------------------------
    //            SIGNALS
    // ------------------------------
    dontTouch(io.i_miss)
    dontTouch(io.b_acc)
    dontTouch(io.b_fetch)
    dontTouch(io.b_move)
    dontTouch(io.i_fetch_end)
    dontTouch(io.i_move_end)
    dontTouch(io.i_use_end)

    dontTouch(w_move)
    dontTouch(w_acc_tag)

    // ------------------------------
    //         DATA FOOTPRINT
    // ------------------------------

    // ------------------------------
    //         MEMORY TRACKER
    // ------------------------------
  }  
}

object Ctrl extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Ctrl(PftchConfigBase), args)
}
