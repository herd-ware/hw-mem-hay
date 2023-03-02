/*
 * File: bus.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:10 pm                                       *
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

import herd.common.gen._
import herd.common.mem.mb4s._
import herd.mem.hay.common._
import herd.mem.hay.cache._
import herd.mem.hay.prev._


// ******************************
//             I/Os
// ******************************
class NextCtrlIO (p: CacheParams, nHart: Int) extends FlatSRVIO(p.useField, p.nField) {
  val hart = Output(UInt(log2Ceil(nHart).W))
  val rw = Output(Bool())
  val size = Output(UInt(SIZE.NBIT.W))
  val addr = Output(new AddrBus(p))
}

// ******************************
//          CONTROL BUS
// ******************************
class NextMemCtrlBus(p: NextParams) extends Bundle {
  val hart = UInt(log2Ceil(p.nHart).W)
  val rw = Bool()
  val size = UInt(SIZE.NBIT.W)
  val addr = new AddrBus(p)

  val mtd = if (p.debug) Some(new MtdBus(p.nAddrBit, p.nMem)) else None
}

class NextOpCtrlBus (p: NextParams) extends Bundle {
  val prev = UInt(log2Ceil(p.nPrevPort).W)
  val rw = Bool()
  val addr = new AddrBus(p)

  val mtd = if (p.debug) Some(new MtdBus(p.nAddrBit, p.nMem)) else None
}

class NextReplaceCtrlBus (p: NextParams) extends Bundle {
  val end = Bool()
  val addr = new AddrBus(p)

  val mtd = if (p.debug) Some(new MtdBus(p.nAddrBit, p.nMem)) else None
}