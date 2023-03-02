/*
 * File: configs.scala                                                         *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:57 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay.prev

import chisel3._
import scala.math._

import herd.common.mem.mb4s._


// ******************************
//           PREV UNIT
// ******************************
object PrevUnitConfigBase extends PrevUnitConfig (  
  pPrevBus = Mb4sConfig (
    debug = true,
    readOnly = false,
    nHart = 2,
    nAddrBit = 32,
    useAmo = false,
    nDataByte = 4,

    useField = false,
    nField = 2,
    multiField = true
  ),
  
  debug = true,
  nCbo = 1,

  nNextDataByte = 8,
  useReqReg = true,
  useAccReg = true,
  useAckReg = true,
  nWriteFifoDepth = 2,

  usePftch = true,
  nPftchEntry = 8,

  nMem = 2,

  nSet = 2,
  nLine = 2,
  nData = 2
)

// ******************************
//             PREV 
// ******************************
object PrevConfigBase extends PrevConfig (
  pPrevBus = Array(
    Mb4sConfig (
      debug = true,
      readOnly = false,
      nHart = 2,
      nAddrBit = 32,
      useAmo = false,
      nDataByte = 4,

      useField = false,
      nField = 2,
      multiField = true
    ),
    Mb4sConfig (
      debug = true,
      readOnly = true,
      nHart = 2,
      nAddrBit = 32,
      useAmo = false,
      nDataByte = 4,
  
      useField = false,
      nField = 2,
      multiField = true
    )
  ),

  debug = true,
  nCbo = 1,

  multiField = true,
  nPart = 1,

  nNextDataByte = 8,
  useReqReg = false,
  useAccReg = false,
  useAckReg = false,
  nWriteFifoDepth = 2,

  usePftch = true,
  nPftchEntry = 8,

  nMem = 2,
  nMemReadPort = 2,
  nMemWritePort = 1,

  slctPolicy = "BitPLRU",

  nSet = 2,
  nLine = 2,
  nData = 2
)
