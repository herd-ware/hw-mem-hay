/*
 * File: mem.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:39:58 pm                                       *
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


class MemMux (p: MemParams) extends Module {
  val io = IO(new Bundle {
    val b_rlog = Flipped(Vec(p.nLogRead, new MemReadIO(p)))
    val b_wlog = Flipped(Vec(p.nLogWrite, new MemWriteIO(p)))
    val b_rphy = Vec(p.nPhyRead, new MemReadIO(p))
    val b_wphy = Vec(p.nPhyWrite, new MemWriteIO(p))
  })

  // ******************************
  //              READ
  // ******************************
  if (p.nLogRead == p.nPhyRead) {
    io.b_rlog <> io.b_rphy
  } else {
    // Default
    for (lr <- 0 until p.nLogRead) {
      io.b_rlog(lr) := DontCare
      io.b_rlog(lr).ready := false.B
    }

    for (pr <- 0 until p.nPhyRead) {
      io.b_rphy(pr) := DontCare
      io.b_rphy(pr).valid := false.B
    }

    // Connect request
    val w_rfree = Wire(Vec(p.nLogRead, Vec(p.nPhyRead, Bool())))
    val w_rslct = Wire(Vec(p.nLogRead, UInt(log2Ceil(p.nPhyRead).W)))

    for (pr <- 0 until p.nPhyRead) {
      w_rfree(0)(pr) := true.B
      w_rslct(0) := 0.U
    }

    io.b_rlog(0) <> io.b_rphy(0)

    for (lr <- 1 until p.nLogRead) {
      w_rfree(lr) := w_rfree(lr - 1)
      when (io.b_rlog(lr - 1).valid) {
        w_rfree(lr)(w_rslct(lr - 1)) := false.B
      }
      w_rslct(lr) := PriorityEncoder(~w_rfree(lr).asUInt)

      io.b_rlog(lr).ready := false.B
      when (w_rfree(lr).asUInt.orR) {
        io.b_rlog(lr) <> io.b_rphy(w_rslct(lr))
      }
    }

    // Connect response
    val r_rslct = Reg(Vec(p.nLogRead, UInt(log2Ceil(p.nPhyRead).W)))
    r_rslct := w_rslct

    for (lr <- 0 until p.nLogRead) {
      io.b_rlog(lr).rdata := io.b_rphy(r_rslct(lr)).rdata
    }
  }

  // ******************************
  //             WRITE
  // ******************************
  if (p.nLogWrite == p.nPhyWrite) {
    io.b_wlog <> io.b_wphy
  } else {
    // Default
    for (lw <- 0 until p.nLogWrite) {
      io.b_wlog(lw).ready := false.B
    }

    for (pw <- 0 until p.nPhyWrite) {
      io.b_wphy(pw) := DontCare
      io.b_wphy(pw).valid := false.B
    }

    // Connect request
    val w_wfree = Wire(Vec(p.nLogWrite, Vec(p.nPhyWrite, Bool())))
    val w_wslct = Wire(Vec(p.nLogWrite, UInt(log2Ceil(p.nPhyWrite).W)))

    for (pw <- 0 until p.nPhyWrite) {
      w_wfree(0)(pw) := true.B
      w_wslct(0) := 0.U
    }

    io.b_wlog(0) <> io.b_wphy(0)

    for (lw <- 1 until p.nLogWrite) {
      w_wfree(lw) := w_wfree(lw - 1)
      when (io.b_wlog(lw - 1).valid) {
        w_wfree(lw)(w_wslct(lw - 1)) := false.B
      }
      w_wslct(lw) := PriorityEncoder(~w_wfree(lw).asUInt)

      io.b_wlog(lw).ready := false.B
      when (w_wfree(lw).asUInt.orR) {
        io.b_wlog(lw) <> io.b_wphy(w_wslct(lw))
      }
    }
  }

  // ******************************
  //             DEBUG
  // ******************************
  // TODO: remove
  // dontTouch(io.b_rlog)
  // dontTouch(io.b_rphy)
  // dontTouch(io.b_wlog)
  // dontTouch(io.b_wphy)
}

class Mem (p: MemParams) extends Module {
  val io = IO(new Bundle {
    val b_read = Flipped(Vec(p.nLogRead, new MemReadIO(p)))
    val b_write = Flipped(Vec(p.nLogWrite, new MemWriteIO(p)))
  })

  // ******************************
  //          DATA MEMORY
  // ******************************
  val m_data = SyncReadMem(p.nData, Vec(p.nDataByte, UInt(8.W)))

  // ******************************
  //           PORT MUX
  // ******************************
  val m_mux = Module(new MemMux(p))

  m_mux.io.b_rlog <> io.b_read
  m_mux.io.b_wlog <> io.b_write

  // ******************************
  //              READ
  // ******************************
  val r_roffset = Reg(Vec(p.nPhyRead, UInt(log2Ceil(p.nDataByte).W)))
  val r_rmask = Reg(Vec(p.nPhyRead, UInt(p.nDataByte.W)))

  val w_rdata = Wire(Vec(p.nPhyRead, Vec(p.nDataByte, UInt(8.W))))
  val w_roffset_shift = Wire(Vec(p.nPhyRead, UInt((log2Ceil(p.nDataByte) + 3).W)))
  val w_rdata_shift = Wire(Vec(p.nPhyRead, UInt((p.nDataByte * 8).W)))
  val w_rdata_format = Wire(Vec(p.nPhyRead, Vec(p.nDataByte, UInt(8.W))))

  for (r <- 0 until p.nPhyRead) {
    r_roffset(r) := m_mux.io.b_rphy(r).offset
    r_rmask(r) := m_mux.io.b_rphy(r).mask

    w_rdata(r) := m_data.read(m_mux.io.b_rphy(r).addr)
    w_roffset_shift(r) := (r_roffset(r) << 3.U)
    w_rdata_shift(r) := (w_rdata(r).asUInt >> w_roffset_shift(r))
    for (db <- 0 until p.nDataByte) {
      w_rdata_format(r)(db) := Mux(r_rmask(r)(db), w_rdata_shift(r)((db + 1) * 8 - 1, db * 8), 0.U)
    } 

    m_mux.io.b_rphy(r).ready := true.B
    m_mux.io.b_rphy(r).rdata := w_rdata_format(r)
  }

  // ******************************
  //             WRITE
  // ******************************
  val w_wmask_shift = Wire(Vec(p.nPhyWrite, UInt(p.nDataByte.W)))
  val w_woffset_shift = Wire(Vec(p.nPhyWrite, UInt((log2Ceil(p.nDataByte) + 3).W)))
  val w_wdata_shift = Wire(Vec(p.nPhyWrite, UInt((p.nDataByte * 8).W)))
  val w_wdata_format = Wire(Vec(p.nPhyWrite, Vec(p.nDataByte, UInt(8.W))))

  for (w <- 0 until p.nPhyWrite) {
    w_wmask_shift(w) := (m_mux.io.b_wphy(w).mask << m_mux.io.b_wphy(w).offset)
    w_woffset_shift(w) := (m_mux.io.b_wphy(w).offset << 3.U)
    w_wdata_shift(w) := (m_mux.io.b_wphy(w).wdata.asUInt << w_woffset_shift(w))
    for (db <- 0 until p.nDataByte) {
      w_wdata_format(w)(db) := Mux(w_wmask_shift(w)(db), w_wdata_shift(w)((db + 1) * 8 - 1, db * 8), 0.U)
    } 
  
    m_mux.io.b_wphy(w).ready := true.B
    when (m_mux.io.b_wphy(w).valid) {
      m_data.write(m_mux.io.b_wphy(w).addr, w_wdata_format(w), w_wmask_shift(w).asBools)
    }
  }

  // ******************************
  //             DEBUG
  // ******************************
  // TODO: remove
  // dontTouch(io.b_read)
  // dontTouch(io.b_write)
}


object Mem extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Mem(MemConfigBase), args)
}
