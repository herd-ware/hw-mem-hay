/*
 * File: ctrl.scala                                                            *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-02 01:39:55 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay.cache

import chisel3._
import chisel3.util._

import herd.mem.hay.common._
import herd.common.field._
import herd.common.mem.cbo.{OP, SORT, BLOCK}
import herd.common.mem.replace._


class Ctrl(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(0, p.nDataBit))) else None
    val b_part = if (p.useField) Some(new NRsrcIO(1, p.nField, p.nPart)) else None
    
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(1, p.useField, p.nField, p.nTagBit, p.nSet)))

    val b_acc = Flipped(Vec(p.nAccess, new CacheAccIO(p)))
    val b_rep = Flipped(new CacheRepIO(p))

    val i_pend = Input(Vec(p.nPendingAcc, new CachePendBus(p)))
    val i_end = Input(new CachePendBus(p))
  })  

  // ******************************
  //            STATUS
  // ******************************
  val w_flush = Wire(Vec(p.nLine, Bool()))

  // ******************************
  //        RESOURCE STATUS
  // ******************************
  val m_line = if (p.useField) Some(Module(new Part2Rsrc(1, p.nField, p.nPart, p.nLine))) else None

  if (p.useField) {
    m_line.get.io.b_part <> io.b_part.get
  }
  
  // ******************************
  //        CONTROL REGISTERS
  // ******************************
  val init_line = Wire(Vec(p.nLine, new CacheEntryBus(p)))

  for (l <- 0 until p.nLine) {
    init_line(l) := DontCare
    init_line(l).vtag := false.B
    init_line(l).vline := false.B
  }

  val r_line = RegInit(init_line) 

  // ******************************
  //            ACCESS
  // ******************************
  // ------------------------------
  //       CHECK DATA PRESENCE
  // ------------------------------
  val w_acc_valid = Wire(Vec(p.nAccess, Vec(p.nLine, Bool())))
  val w_acc_use_line = Wire(Vec(p.nAccess, Vec(p.nLine, Bool())))
  val w_acc_here = Wire(Vec(p.nAccess, Bool()))
  val w_acc_av = Wire(Vec(p.nAccess, Bool()))
  val w_acc_line = Wire(Vec(p.nAccess, UInt(p.nLine.W)))

  for (a <- 0 until p.nAccess) {
    for (l <- 0 until p.nLine) {
      if (p.useField) {
        w_acc_valid(a)(l) := io.b_acc(a).valid & m_line.get.io.b_rsrc.state(l).valid & (io.b_acc(a).field.get === m_line.get.io.b_rsrc.state(l).field)
      } else {
        w_acc_valid(a)(l) := io.b_acc(a).valid 
      }
      w_acc_use_line(a)(l) := w_acc_valid(a)(l) & ~w_flush(l)
    }
  }

  for (a <- 0 until p.nAccess) {
    w_acc_here(a) := false.B
    w_acc_av(a) := false.B
    w_acc_line(a) := 0.U
    for (l <- 0 until p.nLine) {
      when (r_line(l).vtag & (r_line(l).tag === io.b_acc(a).tag) & w_acc_use_line(a)(l)) {
        w_acc_here(a) := true.B
        w_acc_av(a) := r_line(l).vline
        w_acc_line(a) := l.U
      }
    }
    
    io.b_acc(a) := DontCare
    io.b_acc(a).ready := true.B
    io.b_acc(a).found := w_acc_here(a)
    io.b_acc(a).av := w_acc_av(a)
    io.b_acc(a).line := w_acc_line(a)
  }

  // ******************************
  //            REPLACE
  // ******************************
  // ------------------------------
  //       DETECT IN-USE LINE
  // ------------------------------
  val w_use = Wire(Vec(p.nLine, Bool()))
  for (l <- 0 until p.nLine) {
    w_use(l) := false.B
    for (a <- 0 until p.nAccess) {
      when(io.b_acc(a).valid & w_acc_here(a) & (l.U === w_acc_line(a))) {
        w_use(l) := true.B
      }
    }
    for (a <- 0 until p.nPendingAcc) {
      when(io.i_pend(a).valid & (l.U === io.i_pend(a).line)) {
        w_use(l) := true.B
      }
    }
  }

  // ------------------------------
  //      CHECK DATA PRESENCE
  // ------------------------------
  val w_rep_av = Wire(Vec(p.nLine, Bool()))
  val w_rep_use_line = Wire(Vec(p.nLine, Bool()))
  val w_rep_here = Wire(Bool())
  val w_rep_line = Wire(UInt(log2Ceil(p.nLine).W))

  for (l <- 0 until p.nLine) {
    if (p.useField) {
      w_rep_av(l) := io.b_rep.valid & m_line.get.io.b_rsrc.state(l).valid & (io.b_rep.field.get === m_line.get.io.b_rsrc.state(l).field)
    } else {
      w_rep_av(l) := io.b_rep.valid
    }
    w_rep_use_line(l) := w_rep_av(l) & ~w_flush(l)
  }

  w_rep_here := false.B
  w_rep_line := 0.U
  for (l <- 0 until p.nLine) {
    when(r_line(l).vtag & io.b_rep.valid & (r_line(l).tag === io.b_rep.tag) & w_rep_use_line(l)) {
      w_rep_here := true.B
      w_rep_line := l.U
    }
  }

  // ------------------------------
  //         AVAILABLE LINE
  // ------------------------------
  val w_line_av = Wire(Vec(p.nLine, Bool()))

  for (l <- 0 until p.nLine) {
    w_line_av(l) := ~w_use(l) & ~(r_line(l).vtag & ~r_line(l).vline) & (~io.b_rep.empty | ~r_line(l).vtag)
  }

  // ------------------------------
  //            POLICY
  // ------------------------------
  val w_rep_done = Wire(Bool())
  val w_rep_new_line = Wire(UInt(log2Ceil(p.nLine).W))

  val m_pol = p.slctPolicy match {
    case "LRU"      => Module(new LruPolicy(p.useField, p.nField, p.nAccess, p.nLine))
    case "BitPLRU"  => Module(new BitPLruPolicy(p.useField, p.nField, p.nAccess, p.nLine))
    case "Direct"   => Module(new DirectPolicy(p.useField, p.nField, p.nAccess, p.nLine))
    case "PDirect"  => Module(new PDirectPolicy(p.useField, p.nField, p.nAccess, p.nLine))
    case _          => Module(new BitPLruPolicy(p.useField, p.nField, p.nAccess, p.nLine))
  }

  for (l <- 0 until p.nLine) {
    m_pol.io.b_line(l).av := w_line_av(l)
    m_pol.io.b_line(l).flush := w_flush(l)
  }

  for (a <- 0 until p.nAccess) {
    m_pol.io.b_acc(a).valid := w_acc_here(a)
    m_pol.io.b_acc(a).line := w_acc_line(a)
  }

  m_pol.io.b_rep.valid := io.b_rep.valid & ~io.b_rep.check & ~w_rep_here
  m_pol.io.b_rep.fixed := io.b_rep.tag(log2Ceil(p.nLine) - 1,0)

  if (p.useField) {
    m_pol.io.b_rsrc.get <> m_line.get.io.b_rsrc
    m_pol.io.b_rep.field.get := io.b_rep.field.get
  }

  w_rep_done := m_pol.io.b_rep.done
  w_rep_new_line := m_pol.io.b_rep.line

  io.b_rep := DontCare
  io.b_rep.ready := true.B
  io.b_rep.found := w_rep_here
  io.b_rep.alloc := w_rep_done
  io.b_rep.line := Mux(w_rep_here, w_rep_line, w_rep_new_line)

  // ------------------------------
  //    UPDATE CONTROL REGISTERS
  // ------------------------------
  for (l <- 0 until p.nLine) {
    when(w_flush(l) & r_line(l).vtag & r_line(l).vline & ~w_use(l)) {
      r_line(l).vtag := false.B
      r_line(l).vline := false.B
    }.elsewhen (io.i_end.valid & (l.U === io.i_end.line)) {
      r_line(l).vline := true.B
    }.elsewhen (io.b_rep.valid & w_rep_done & ~w_rep_here & (l.U === w_rep_new_line)) {
      r_line(l).vtag := true.B
      r_line(l).vline := false.B
      r_line(l).tag := io.b_rep.tag
    }
  }

  // ******************************
  //              CBO
  // ******************************
  // ------------------------------
  //            PRESENCE
  // ------------------------------
  val w_cbo_av = Wire(Vec(p.nCbo, Vec(p.nLine, Bool())))
  val w_cbo_here = Wire(Vec(p.nCbo, Bool()))
  val w_cbo_line = Wire(Vec(p.nCbo, UInt(p.nLine.W)))

  for (c <- 0 until p.nCbo) {
    w_cbo_here(c) := false.B
    w_cbo_line(c) := 0.U

    for (l <- 0 until p.nLine) {
      if (p.useField) {
        w_cbo_av(c)(l) := io.b_cbo(c).valid & m_line.get.io.b_rsrc.state(l).valid & (io.b_cbo(c).field.get === m_line.get.io.b_rsrc.state(l).field) 
      } else {
        w_cbo_av(c)(l) := io.b_cbo(c).valid
      }

      when (r_line(l).vtag & w_cbo_av(c)(l) & (r_line(l).tag === io.b_cbo(c).tag)) {
        w_cbo_here(c) := true.B
        w_cbo_line(c) := l.U                      
      }
    }
  }

  // ------------------------------
  //             READY
  // ------------------------------
  for (c <- 0 until p.nCbo) {
    io.b_cbo(c).ready := true.B

    when ((io.b_cbo(c).op === OP.INVAL) | (io.b_cbo(c).op === OP.FLUSH)) {
      when (io.b_cbo(c).block === BLOCK.LINE) {
        io.b_cbo(c).ready := ~w_cbo_here(c) | (~r_line(w_cbo_line(c)).vtag & ~r_line(w_cbo_line(c)).vline & m_pol.io.b_line(w_cbo_line(c)).free)
      }.otherwise {
        when ((io.b_cbo(c).block === BLOCK.FULL) | (io.b_cbo(c).block === BLOCK.SET)) {
          for (l <- 0 until p.nLine) {
            when (w_cbo_av(c)(l) & (r_line(l).vtag | r_line(l).vline | ~m_pol.io.b_line(l).free)) {
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
    for (l <- 0 until p.nLine) {
      m_line.get.io.b_rsrc.state(l).free := ~r_line(l).vtag & ~r_line(l).vline & m_pol.io.b_line(l).free
    }
  }  

  // ******************************
  //             FLUSH
  // ******************************
  for (l <- 0 until p.nLine) {
    if (p.useField) w_flush(l) := m_line.get.io.b_rsrc.state(l).flush else w_flush(l) := false.B

    for (c <- 0 until p.nCbo) {
      when ((io.b_cbo(c).op === OP.INVAL) | (io.b_cbo(c).op === OP.FLUSH)) {
        when (w_cbo_av(c)(l) & ((io.b_cbo(c).block === BLOCK.FULL) | (io.b_cbo(c).block === BLOCK.SET) | (w_cbo_here(c) & (io.b_cbo(c).block === BLOCK.LINE) & (l.U === w_cbo_line(c))))) {        
          w_flush(l) := true.B
        }
      }
    }
  }

  // ******************************
  //            DEBUG
  // ******************************
  if (p.debug) {
    // ------------------------------
    //            SIGNALS
    // ------------------------------
    dontTouch(r_line)

    // ------------------------------
    //         DATA FOOTPRINT
    // ------------------------------

    // ------------------------------
    //         MEMORY TRACKER
    // ------------------------------
  }
}

object Ctrl extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Ctrl(CacheConfigBase), args)
}
