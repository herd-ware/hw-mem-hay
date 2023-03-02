/*
 * File: params.scala                                                          *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:41:01 pm                                       *
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
import herd.mem.hay.cache._


// ******************************
//       DEPEND PARAMETERS
// ******************************
trait DependParams extends CacheParams {
  def debug: Boolean
  def nHart: Int
  def nAddrBit: Int
  def nCbo: Int
  
  def useField: Boolean
  def nField: Int
  def multiField: Boolean
  def nPart: Int

  def nAccess: Int
  def nReadPort: Int
  def nWritePort: Int
  def nPendingAcc: Int

  def slctPolicy: String
  def nData: Int
  def nDataByte: Int
  def nLine: Int
  def nSet: Int
  def nTagBit: Int

  def nMem: Int
  def nMemReadPort: Int
  def nMemWritePort: Int

  def usePftch: Boolean
  def nPftchEntry: Int
}

// ******************************
//      PREV UNIT PARAMETERS
// ******************************
trait PrevUnitParams extends DependParams {
  def pPrevBus: Mb4sParams  

  def debug: Boolean
  def nAddrBit: Int = pPrevBus.nAddrBit
  def readOnly: Boolean = pPrevBus.readOnly
  def nHart: Int = pPrevBus.nHart
  def useAmo: Boolean = pPrevBus.useAmo
  def nOpBit: Int = pPrevBus.nOpBit
  def nCbo: Int 

  def useField: Boolean = pPrevBus.useField
  def nField: Int = pPrevBus.nField
  def multiField: Boolean = pPrevBus.multiField
  def nPart: Int = 1

  def nPrevDataByte: Int = pPrevBus.nDataByte
  def nPrevDataBit: Int = nPrevDataByte * 8
  def nNextDataByte: Int
  def nNextDataBit: Int = nNextDataByte * 8
  def useReqReg: Boolean
  def useAccReg: Boolean
  def useAckReg: Boolean
  def nStage: Int = {
    var s: Int = 2
    if (useAccReg) s = s + 1
    if (useAckReg) s = s + 1
    return s
  }
  def nWriteFifoDepth: Int

  def usePftch: Boolean
  def nPftchEntry: Int

  def useRsv: Boolean = useAmo
  def nAccess: Int = 1
  def nReadPort: Int = 1
  def nWritePort: Int = if (!readOnly) 1 else 0
  def nPendingAcc: Int = {
    var pa: Int = nFieldSlct
    if (useAccReg) pa = pa + nFieldSlct
    if (useAckReg && !readOnly) pa = pa + nFieldSlct
    return pa
  }
  def nMem: Int
  def nMemReadPort: Int = 1
  def nMemWritePort: Int = 1

  def slctPolicy: String = ""
  def nSet: Int
  def nLine: Int
  def nData: Int
  def nDataByte: Int = max(nPrevDataByte, nNextDataByte)
  def nTagBit: Int = nAddrBit - log2Ceil(nSet) - log2Ceil(nLineByte)
}

case class PrevUnitConfig (
  pPrevBus: Mb4sParams,

  debug: Boolean,
  nCbo: Int,

  nNextDataByte: Int,
  useReqReg: Boolean,
  useAccReg: Boolean,
  useAckReg: Boolean,
  nWriteFifoDepth: Int,

  usePftch: Boolean,
  nPftchEntry: Int,

  nMem: Int,

  nSet: Int,
  nLine: Int,
  nData: Int
) extends PrevUnitParams

// ******************************
//         PREV PARAMETERS
// ******************************
trait PrevParams extends DependParams {
  def pPrevBus: Array[Mb4sParams]
  def nPrevPort: Int = pPrevBus.size

  def debug: Boolean
  def nAddrBit: Int = {
    var nbit: Int = pPrevBus(0).nAddrBit
    for (prev <- pPrevBus) {
      if (prev.nAddrBit > nbit) {
        nbit = prev.nAddrBit
      }
    }
    return nbit
  }
  def readOnly: Boolean = {
    var ro: Boolean = true
    for (prev <- pPrevBus) {
      if (!prev.readOnly) {
        ro = false
      }
    }
    return ro
  }
  def nHart: Int = {
    var nhart: Int = pPrevBus(0).nHart
    for (prev <- pPrevBus) {
      if (prev.nHart > nhart) {
        nhart = prev.nHart
      }
    }
    return nhart
  }
  def useAmo: Boolean = {
    var amo: Boolean = false
    for (prev <- pPrevBus) {
      if (prev.useAmo) {
        amo = true
      }
    }
    return amo
  }
  def nOpBit: Int = {
    if (!readOnly && useAmo) {
      return OP.NBIT
    } else {
      return 1 
    }
  }
  def nCbo: Int 

  def useField: Boolean = {
    var use: Boolean = pPrevBus(0).useField
    for (prev <- pPrevBus) {
      require((prev.useField == use), "All the previous memories must use fields to allow its support.")
    }
    return use
  }
  def nField: Int = {
    var nfield: Int = pPrevBus(0).nField
    for (prev <- pPrevBus) {
      if (prev.nField > nfield) {
        nfield = prev.nField
      }
    }
    return nfield
  }
  def nPart: Int
  def multiField: Boolean
  override def useFieldTag: Boolean = {
    for (prev <- pPrevBus) {
      if (prev.useFieldSlct) {
        return false
      }
    }
    return (useField && !multiField)    
  }
  override def useFieldSlct: Boolean = {
    for (prev <- pPrevBus) {
      if (prev.useFieldSlct) {
        return true
      }
    }
    return (useField && multiField)  
  }

  def nPrevDataByte: Int = {
    var nbyte: Int = pPrevBus(0).nDataByte
    for (prev <- pPrevBus) {
      if (prev.nDataByte > nbyte) {
        nbyte = prev.nDataByte
      }
    }
    return nbyte
  }
  def nPrevDataBit: Int = nPrevDataByte * 8
  def nNextDataByte: Int
  def nNextDataBit: Int = nNextDataByte * 8
  def useReqReg: Boolean
  def useAccReg: Boolean
  def useAckReg: Boolean
  def nStage: Int = {
    var s: Int = 2
    if (useAccReg) s = s + 1
    if (useAckReg && !readOnly) s = s + 1
    return s
  }
  def nWriteFifoDepth: Int

  def usePftch: Boolean
  def nPftchEntry: Int

  def useRsv: Boolean = useAmo
  def nAccess: Int = nPrevPort
  def nReadPort: Int = nPrevPort
  def nWritePort: Int = if (!readOnly) nPrevPort else 0
  def nPendingAcc: Int = {    
    var pa: Int = 0
    for (prev <- pPrevBus) {
      pa = pa + prev.nFieldSlct
      if (useAccReg) pa = pa + prev.nFieldSlct
      if (useAckReg && !readOnly) pa = pa + prev.nFieldSlct
    }
    return pa
  }
  def nMem: Int
  def nMemReadPort: Int
  def nMemWritePort: Int

  def slctPolicy: String
  def nSet: Int
  def nLine: Int
  def nData: Int
  def nDataByte: Int = max(nPrevDataByte, nNextDataByte)
  def nTagBit: Int = nAddrBit - log2Ceil(nSet) - log2Ceil(nLineByte)

  def pPrev: Array[PrevUnitParams] = {
    var p = new Array[PrevUnitParams](nPrevPort)
    for (prev <- 0 until nPrevPort) {
      p(prev) = new PrevUnitConfig (
        pPrevBus = pPrevBus(prev),

        debug = debug,
        nCbo = nCbo,

        nNextDataByte = nNextDataByte,
        useReqReg = useReqReg,
        useAccReg = useAccReg,
        useAckReg = useAckReg,
        nWriteFifoDepth = nWriteFifoDepth,

        usePftch = usePftch,
        nPftchEntry = nPftchEntry,

        nMem = nMem,
        
        nSet = nSet,
        nLine = nLine,
        nData = nData
      )      
    }
    return p
  }
}

case class PrevConfig (
  pPrevBus: Array[Mb4sParams],

  debug: Boolean,
  nCbo: Int,

  multiField: Boolean,
  nPart: Int,

  nNextDataByte: Int,
  useReqReg: Boolean,
  useAccReg: Boolean,
  useAckReg: Boolean,
  nWriteFifoDepth: Int,

  usePftch: Boolean,
  nPftchEntry: Int,

  nMem: Int,
  nMemReadPort: Int,
  nMemWritePort: Int,

  slctPolicy: String,
  nSet: Int,
  nLine: Int,
  nData: Int
) extends PrevParams