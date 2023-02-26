/*
 * File: params.scala                                                          *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:41:23 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay

import chisel3._
import chisel3.util._
import scala.math._

import herd.common.mem.mb4s._
import herd.mem.hay.cache._
import herd.mem.hay.prev._
import herd.mem.hay.next._
import herd.mem.hay.pftch._


// ******************************
//         GAVE PARAMETERS 
// ******************************
trait HayParams extends PrevParams 
                  with NextParams
                  with PftchParams {
  // ------------------------------
  //   PREVIOUS MEMORY PARAMETERS
  // ------------------------------
  def pPrevBus: Array[Mb4sParams]
  override def nPrevPort: Int = pPrevBus.size

  // ------------------------------
  //       GLOBAL PARAMETERS
  // ------------------------------
  def debug: Boolean
  override def nAddrBit: Int = {
    var nbit: Int = pPrevBus(0).nAddrBit
    for (prev <- pPrevBus) {
      if (prev.nAddrBit > nbit) {
        nbit = prev.nAddrBit
      }
    }
    return nbit
  }
  override def readOnly: Boolean = {
    var ro: Boolean = true
    for (prev <- pPrevBus) {
      if (!prev.readOnly) {
        ro = false
      }
    }
    return ro
  }
  override def nHart: Int = {
    var nhart: Int = pPrevBus(0).nHart
    for (prev <- pPrevBus) {
      if (prev.nHart > nhart) {
        nhart = prev.nHart
      }
    }
    return nhart
  }
  def nCbo: Int 

  // ------------------------------
  //         DOME PARAMETERS
  // ------------------------------
  override def useDome: Boolean = {
    var use: Boolean = pPrevBus(0).useDome
    for (prev <- pPrevBus) {
      require((prev.useDome == use), "All the previous memories must use domes to allow its support.")
    }
    return use
  }
  override def nDome: Int = {
    var ndome: Int = pPrevBus(0).nDome
    for (prev <- pPrevBus) {
      if (prev.nDome > ndome) {
        ndome = prev.nDome
      }
    }
    return ndome
  }
  def multiDome: Boolean
  override def useDomeTag: Boolean = {
    for (prev <- pPrevBus) {
      if (prev.useDomeSlct) {
        return false
      }
    }
    return (useDome && !multiDome)    
  }
  override def useDomeSlct: Boolean = {
    for (prev <- pPrevBus) {
      if (prev.useDomeSlct) {
        return true
      }
    }
    return (useDome && multiDome)  
  }
  def nPart: Int

  // ------------------------------
  //     CONTROLLER PARAMETERS
  // ------------------------------
  override def nPrevDataByte: Int = {
    var nbyte: Int = pPrevBus(0).nDataByte
    for (prev <- pPrevBus) {
      if (prev.nDataByte > nbyte) {
        nbyte = prev.nDataByte
      }
    }
    return nbyte
  }
  override def nPrevDataBit: Int = nPrevDataByte * 8
  def nNextDataByte: Int
  override def nNextDataBit: Int = nNextDataByte * 8
  def useReqReg: Boolean
  def useAccReg: Boolean
  def useAckReg: Boolean
  def nWriteFifoDepth: Int
  def nNextFifoDepth: Int
  def nNextLatency: Int
  override def nAccess: Int = nPrevPort
  override def nReadPort: Int = nPrevPort
  override def nWritePort: Int = {
    var nport: Int = 1
    if (!readOnly) nport = nport + nPrevPort
    if(usePftch) nport = nport + 1
    return nport
  }
  override def nPendingAcc: Int = {    
    var pa: Int = 0
    for (prev <- pPrevBus) {
      pa = pa + prev.nDomeSlct
      if (useAccReg) pa = pa + prev.nDomeSlct
      if (useAckReg && !readOnly) pa = pa + prev.nDomeSlct
    }
    return pa
  }

  // ------------------------------
  //     PREFETCHER PARAMETERS
  // ------------------------------
  def usePftch: Boolean
  def nPftchEntry: Int
  def nPftchEntryAcc: Int
  def nPftchMemRead: Int
  def nPftchMemWrite: Int
  def nPftchFifoDepth: Int = nNextFifoDepth

  // ------------------------------
  //   PHYSICAL MEMORY PARAMETERS
  // ------------------------------ 
  def nMem: Int
  def nMemReadPort: Int
  def nMemWritePort: Int

  // ------------------------------
  //        CACHE PARAMETERS
  // ------------------------------ 
  override def useRsv: Boolean = useAmo
  def slctPolicy: String
  def nSet: Int
  def nLine: Int
  def nData: Int
  override def nDataByte: Int = max(nPrevDataByte, nNextDataByte)
  override def nTagBit: Int = nAddrBit - log2Ceil(nSet) - log2Ceil(nLineByte)

  // ------------------------------
  //     NEXT MEMORY PARAMETERS
  // ------------------------------
  override def pNextBus: Mb4sParams = new Mb4sConfig (
    debug = debug,
    readOnly = readOnly,
    nHart = nHart,
    nAddrBit = nAddrBit,
    useAmo = false,
    nDataByte = nNextDataByte,
    useDome =  useDome,
    nDome = nDome,
    multiDome = multiDome    
  )

  def pMux: Mb4sCrossbarParams = new Mb4sCrossbarConfig (
    pMaster = Array(pNextBus, pNextBus),
    useMem = false,
    pMem = Array(),
    nDefault = 0,
    nBus = 1,

    debug = debug,
    multiDome = multiDome,
    nDepth = nNextFifoDepth,
    useDirect = false
  )
}

// ******************************
//          GAVE CONFIG 
// ******************************
case class HayConfig (
  // ------------------------------
  //       GLOBAL PARAMETERS
  // ------------------------------
  debug: Boolean,
  nCbo: Int,

  // ------------------------------
  //   PREVIOUS MEMORY PARAMETERS
  // ------------------------------
  pPrevBus: Array[Mb4sParams],

  // ------------------------------
  //         DOME PARAMETERS
  // ------------------------------
  multiDome: Boolean,
  nPart: Int,

  // ------------------------------
  //     CONTROLLER PARAMETERS
  // ------------------------------
  nNextDataByte: Int,
  useReqReg: Boolean,
  useAccReg: Boolean,
  useAckReg: Boolean,
  nWriteFifoDepth: Int,
  nNextFifoDepth: Int,
  nNextLatency: Int,

  // ------------------------------
  //     PREFETCHER PARAMETERS
  // ------------------------------
  usePftch: Boolean,
  nPftchEntry: Int,
  nPftchEntryAcc: Int,
  nPftchMemRead: Int,
  nPftchMemWrite: Int,

  // ------------------------------
  //   PHYSICAL MEMORY PARAMETERS
  // ------------------------------
  nMem: Int,
  nMemReadPort: Int,
  nMemWritePort: Int,

  // ------------------------------
  //        CACHE PARAMETERS
  // ------------------------------ 
  slctPolicy: String,
  nSet: Int,
  nLine: Int,
  nData: Int
) extends HayParams