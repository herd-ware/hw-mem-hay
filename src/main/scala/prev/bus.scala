/*
 * File: bus.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:55 pm                                       *
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
import scala.math._

import herd.common.mem.mb4s._
import herd.mem.hay.common._
import herd.mem.hay.cache._


// ******************************
//         CONTROL BUS
// ******************************
class InfoBus (p: PrevUnitParams) extends Bundle {
  val hart = UInt(log2Ceil(p.nHart).W)
  val zero = Bool()
  val rsv = if (p.useAmo) Some(Bool()) else None
}

class OpBus(useAmo: Boolean, nOpBit: Int, nDataByte: Int) extends Bundle {
  val op = UInt(nOpBit.W)
  val amo = if (useAmo) Some(UInt(AMO.NBIT.W)) else None
  val size = UInt(SIZE.NBIT.W)
  val mask = UInt(nDataByte.W)

  def ro: Bool = {
    if (useAmo) {
      return (op === OP.R) | (op === OP.LR)
    } else {
      return (op === 0.B)
    }
  }
  def wo: Bool = {
    if (useAmo) {
      return (op === OP.W) | (op === OP.SC)
    } else {
      return (op === 1.B)
    }
  }
  def a: Bool = {
    if (useAmo) {
      return (op === OP.AMO)
    } else {
      return 0.B
    }
  }
  def ra: Bool = this.ro | this.a
  def wa: Bool = this.wo | this.a
}

class PrefetcherBus(nPftchEntry: Int) extends Bundle {
  val use = Bool()
  val entry = UInt(log2Ceil(nPftchEntry).W)
}

// ******************************
//           STAGE BUS
// ******************************
class PrevUnitCtrlBus (p: PrevUnitParams) extends Bundle {
  val info = new InfoBus(p)
  val addr = new AddrBus(p)
  val op = new OpBus(p.useAmo, p.nOpBit, p.nDataByte)
  val pftch = if (p.usePftch) Some(new PrefetcherBus(p.nPftchEntry)) else None

  val mtd = if (p.debug) Some(new MtdBus(p.nAddrBit, p.nMem)) else None

  def setPort(in: Mb4sReqBus): Unit = {
    info.hart := in.hart
    info.zero := false.B
    if (p.useAmo) info.rsv.get := DontCare

    if (!p.readOnly) op.op := in.op else op.op := OP.R
    if (p.useAmo) op.amo.get := in.amo.get
    op.size := in.size
    op.mask := SIZE.toMask(p.nDataByte, in.size)

    if (p.nTagBit > 0)    addr.tag := in.addr(p.nAddrBit - 1, p.nAddrBit - p.nTagBit) else addr.tag := 0.U
    if (p.nSet > 1)       addr.set := in.addr(log2Ceil(p.nLineByte) + log2Ceil(p.nSet) - 1, log2Ceil(p.nLineByte)) else addr.set := 0.U
    if (p.nData > 1)      addr.data := in.addr(log2Ceil(p.nLineByte) - 1, log2Ceil(p.nDataByte)) else addr.data := 0.U
    if (p.nDataByte > 1)  addr.offset := in.addr(log2Ceil(p.nDataByte) - 1, 0) else addr.offset := 0.U
                          addr.line := 0.U

    if (p.usePftch) {
      pftch.get.use := false.B
      pftch.get.entry := 0.U
    }    

    if (p.debug) {
      mtd.get.addr := in.addr
      mtd.get.mem := addr.mem()
    }
  }
}

class WriteDataBus (p: PrevUnitParams) extends Bundle {
  val sreg = UInt(p.nDataBit.W)
  val smem = UInt(p.nDataBit.W)
}

// ******************************
//            DEPEND
// ******************************
class DependRegBus (p: DependParams) extends Bundle {
  val valid = Bool()
  val lock = Bool()
  val hart = UInt(log2Ceil(p.nHart).W)
  val dome = if (p.useDome) Some(UInt(log2Ceil(p.nDome).W)) else None
  val rw = Bool()
  val addr = new AddrBus(p)
  val pftch = if (p.usePftch) Some(new PrefetcherBus(p.nPftchEntry)) else None
}

class DependStageIO (p: DependParams) extends Bundle {
  val reg = Output(new DependRegBus(p))
  val lock = Input(Bool())
}

// ******************************
//           RESERVE
// ******************************
class ReserveIO(nHart: Int, nAddrBit: Int) extends Bundle {
  val valid = Input(Bool())
  val hart = Input(UInt(log2Ceil(nHart).W))
  val rw = Input(Bool())
  val rsv = Input(Bool())
  val size = Input(UInt(SIZE.NBIT.W))
  val addr = Input(UInt(nAddrBit.W))

  val ready = Output(Bool())
}

class ReserveEntryBus(p: PrevParams) extends Bundle {
  val valid = Bool()
  val size = UInt(SIZE.NBIT.W)
  val addr = UInt(p.nAddrBit.W)
}

// ******************************
//             ZERO
// ******************************
class ZeroIO (p: CacheParams) extends Bundle {
  val ready = Input(Bool())
  val valid = Output(Bool())
  val hart = Output(UInt(log2Ceil(p.nHart).W))
  val addr = Output(new AddrBus(p))
}

class ZeroEntryBus(p: CacheParams) extends Bundle {
  val valid = Bool()
  val hart = UInt(log2Ceil(p.nHart).W)
  val addr = new AddrBus(p)
}
