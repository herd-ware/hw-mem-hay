/*
 * File: bus.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:35 pm                                       *
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
import herd.common.mem.mb4s._
import herd.mem.hay.common._
import herd.mem.hay.cache._
import herd.mem.hay.prev._


// ******************************
//             I/Os
// ******************************
class PftchAccIO(p: CacheParams, nPftchEntry: Int) extends FlatSRVIO(p.useDome, p.nDome) {
  val hart = Output(UInt(log2Ceil(p.nHart).W))
  val rw = Output(Bool())
  val check = Output(Bool())
  val tag = Output(UInt(p.nTagBit.W))
  val set = Output(UInt(log2Ceil(p.nSet).W))

  val found = Input(Bool())      
  val av = Input(Bool())     
  val entry = Input(UInt(log2Ceil(nPftchEntry).W))
}

class PftchReadIO(p: CacheParams, nPftchEntry: Int) extends FlatSRVIO(p.useDome, p.nDome) {
  val mask = Output(UInt(p.nDataByte.W))
  val entry = Output(UInt(log2Ceil(nPftchEntry).W))
  val data = Output(UInt(log2Ceil(p.nData).W))
  val offset = Output(UInt(log2Ceil(p.nDataByte).W))
  
  val rdata = Input(Vec(p.nDataByte, UInt(8.W)))
}

class PftchWriteIO(p: CacheParams, nPftchEntry: Int) extends FlatSRVIO(p.useDome, p.nDome) {
  val mask = Output(UInt(p.nDataByte.W))
  val entry = Output(UInt(log2Ceil(nPftchEntry).W))
  val data = Output(UInt(log2Ceil(p.nData).W))
  val offset = Output(UInt(log2Ceil(p.nDataByte).W))
  val wdata = Output(Vec(p.nDataByte, UInt(8.W)))  
}

class PftchFetchIO(p: PftchParams) extends Bundle {
  val valid = Output(Bool())
  val dome = if (p.useDome) Some(Output(UInt(log2Ceil(p.nDome).W))) else None
  val tag = Output(UInt(p.nPftchTagBit.W))

  val ready = Input(Bool())
  val here = Input(Bool())
  val entry = Input(UInt(log2Ceil(p.nPftchEntry).W))
}

class PftchMoveIO(p: PftchParams) extends Bundle {
  val valid = Output(Bool())
  val dome = if (p.useDomeSlct) {
    Some(Output(UInt(log2Ceil(p.nDome).W)))
   } else if (p.useDomeTag) {
    Some(Input(UInt(log2Ceil(p.nDome).W))) 
   } else None
  
  val ready = Input(Bool())
  val used = Input(Bool())
  val entry = Input(UInt(log2Ceil(p.nPftchEntry).W))
  val tag = Input(UInt(p.nPftchTagBit.W))
}

// ******************************
//          CONTROL BUS
// ******************************
class PftchEntryBus(p: PftchParams) extends Bundle {
  val vtag = Bool()
  val vdata = Bool()
  val old = Bool()
  val used = Bool()
  val in_use = UInt(p.nPftchEntryAcc.W)
  val move = Bool()
  val tag = UInt(p.nPftchTagBit.W)
}

class PftchCtrlBus(p: PftchParams) extends Bundle {
  val hart = UInt(log2Ceil(p.nHart).W)
  //val wentry = Bool()
  val entry = UInt(log2Ceil(p.nPftchEntry).W)
  val addr = new AddrBus(p)
}

class PftchPendBus(p: PftchParams) extends Bundle {
  val valid = Bool()
  val entry = UInt(log2Ceil(p.nPftchEntry).W)
}