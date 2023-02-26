/*
 * File: bus.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:06 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay.common

import chisel3._
import chisel3.util._
import scala.math._

import herd.common.mem.cbo.{OP, SORT, BLOCK, CboIO => GlobCboIO}
import herd.mem.hay.cache._ 


// ******************************
//            ADDRESS
// ******************************
class AddrBus (p: CacheParams) extends Bundle {
  val tag = UInt(p.nTagBit.W)
  val set = UInt(log2Ceil(p.nSet).W)
  val data = UInt(log2Ceil(p.nData).W)
  val offset = UInt(log2Ceil(p.nDataByte).W)
  val line = UInt(log2Ceil(p.nLine).W)
  
  def toFull(): UInt = {
    return Cat(tag, set, data, offset)
  }

  def fromFull(addr: UInt) = {
    if (p.nDataByte > 1) offset := addr(log2Ceil(p.nDataByte) - 1, 0) else offset := 0.U
    if (p.nData > 1) data := addr(log2Ceil(p.nLineByte) - 1, log2Ceil(p.nDataByte)) else data := 0.U
    if (p.nSet > 1) set := addr(log2Ceil(p.nSet) + log2Ceil(p.nLineByte) - 1, log2Ceil(p.nLineByte)) else set := 0.U
    if (p.nTagBit > 0) tag := addr(p.nTagBit + log2Ceil(p.nSet) + log2Ceil(p.nLineByte) - 1, log2Ceil(p.nSet) + log2Ceil(p.nLineByte)) else tag := 0.U
  }

  def toLineAddr(): UInt = {
    return Cat(tag, set)
  }

  def mem(): UInt = {
    if ((p.nMem > 1) || (p.nSet > 1)) {
      return set(log2Ceil(p.nSet) - 1, log2Ceil(p.nSet) - 1 - max(0, log2Ceil(p.nMem) - 1))      
    } else {
      return 0.U
    }
  }

  def memdata(): UInt = {
    val w_addr = Cat(set, line, data)
    if (p.nMemData > 1) {
      return w_addr(log2Ceil(p.nMemData) - 1, 0)
    } else {
      return 0.U
    }
  }
}

// ******************************
//              CBO
// ******************************
class CboIO (nHart: Int, useDome: Boolean, nDome: Int, nTagBit: Int, nSet: Int) extends Bundle {
  val ready = Input(Bool())
  val valid = Output(Bool())
  val hart = Output(UInt(log2Ceil(nHart).W))
  val dome = if (useDome) Some(Output(UInt(log2Ceil(nDome).W))) else None
  val op = Output(UInt(OP.NBIT.W))
  val sort = Output(UInt(SORT.NBIT.W))
  val block = Output(UInt(BLOCK.NBIT.W))
  val tag = Output(UInt(nTagBit.W))
  val set = Output(UInt(log2Ceil(nSet).W))

  def fromGlob (glob: GlobCboIO, nAddrBit: Int) = {
    valid := glob.valid
    hart := glob.hart
    if (useDome) dome.get := glob.dome.get
    op := glob.op
    sort := glob.sort
    block := glob.block
    if (nTagBit > 0) tag := glob.addr(nAddrBit - 1, nAddrBit - nTagBit) else tag := 0.U
    if (nSet > 1) set := glob.addr(nAddrBit - nTagBit - 1, nAddrBit - nTagBit - log2Ceil(nSet)) else set := 0.U
  }

  def cln: Bool = (op === OP.CLEAN) | (op === OP.FLUSH)
  def inv: Bool = (op === OP.INVAL) | (op === OP.FLUSH)
  def zero: Bool = (op === OP.ZERO)
  def hint: Bool = (op === OP.PFTCH)
}

// ******************************
//             MISS
// ******************************
class MissBus(p: CacheParams, nHart: Int) extends Bundle {
  val valid = Bool()
  val hart = UInt(log2Ceil(nHart).W)
  val dome = if (p.useDome) Some(UInt(log2Ceil(p.nDome).W)) else None
  val addr = new AddrBus(p)
}

// ******************************
//     MEMORY TRACKER TO DEBUG
// ******************************
class MtdBus (nAddrBit: Int, nMem: Int) extends Bundle {
  val addr = UInt(nAddrBit.W)
  val mem = UInt(log2Ceil(nMem).W)
}
