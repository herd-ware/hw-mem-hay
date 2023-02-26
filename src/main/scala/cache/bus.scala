/*
 * File: bus.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:39:51 pm                                       *
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

import herd.common.gen._


// ******************************
//            MEMORY
// ******************************
class MemReadIO (p: MemParams) extends Bundle {
  val valid = Output(Bool())
  val mask = Output(UInt(p.nDataByte.W))
  val addr = Output(UInt(log2Ceil(p.nData).W))
  val offset = Output(UInt(log2Ceil(p.nDataByte).W))

  val ready = Input(Bool())
  val rdata = Input(Vec(p.nDataByte, UInt(8.W)))
}

class MemWriteIO (p: MemParams) extends Bundle {
  val valid = Output(Bool())
  val mask = Output(UInt(p.nDataByte.W))
  val addr = Output(UInt(log2Ceil(p.nData).W))
  val offset = Output(UInt(log2Ceil(p.nDataByte).W))
  val wdata = Output(Vec(p.nDataByte, UInt(8.W)))

  val ready = Input(Bool())
}

// ******************************
//            CACHE
// ******************************
class CacheAccIO(p: CacheParams) extends FlatSRVIO(p.useDome, p.nDome) {
  val hart = Output(UInt(log2Ceil(p.nHart).W))
  val rw = Output(Bool())
  val tag = Output(UInt(p.nTagBit.W))
  val set = Output(UInt(log2Ceil(p.nSet).W))

  //val ready = Input(Bool())                             // Request received
  val found = Input(Bool())                               // Tag found  
  val av = Input(Bool())                                  // Data available
  val line = Input(UInt(log2Ceil(p.nLine).W))             // Data Line
}

class CacheRepIO(p: CacheParams) extends FlatSRVIO(p.useDome, p.nDome) {
  val hart = Output(UInt(log2Ceil(p.nHart).W))
  val check = Output(Bool())
  val empty = Output(Bool())
  val tag = Output(UInt(p.nTagBit.W))
  val set = Output(UInt(log2Ceil(p.nSet).W))
  
  //val ready = Input(Bool())                             // Request received
  val found = Input(Bool())                               // Tag found
  val line = Input(UInt(log2Ceil(p.nLine).W))             // Replaced line
  val alloc = Input(Bool())                               // Replace possible
}

class CachePendBus(p: CacheParams) extends Bundle {
  val valid = Bool()
  val line = UInt(log2Ceil(p.nLine).W)
  val set = UInt(log2Ceil(p.nSet).W)
}

// Read port
class CacheReadIO(p: CacheParams) extends FlatSRVIO(p.useDome, p.nDome) {
  val mask = Output(UInt(p.nDataByte.W))
  val offset = Output(UInt(log2Ceil(p.nDataByte).W))
  val data = Output(UInt(log2Ceil(p.nMemData).W))
  val mem = Output(UInt(log2Ceil(p.nMem).W))

  val rdata = Input(Vec(p.nDataByte, UInt(8.W)))
}

// Write port
class CacheWriteIO(p: CacheParams) extends FlatSRVIO(p.useDome, p.nDome) {
  val mask = Output(UInt(p.nDataByte.W))
  val offset = Output(UInt(log2Ceil(p.nDataByte).W))
  val data = Output(UInt(log2Ceil(p.nMemData).W))
  val mem = Output(UInt(log2Ceil(p.nMem).W))
  val wdata = Output(Vec(p.nDataByte, UInt(8.W)))
}

// ******************************
//          CONTROL BUS
// ******************************
class CacheEntryBus(p: CacheParams) extends Bundle {
  val vtag = Bool()
  val vline = Bool()
  val tag = UInt(p.nTagBit.W)
}
