/*
 * File: configs.scala                                                         *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:41:19 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay

import chisel3._
import scala.math._

import herd.common.mem.mb4s._


// ******************************
//          GAVE CONFIG 
// ******************************
object HayConfigBase extends HayConfig (
  // ------------------------------
  //       GLOBAL PARAMETERS
  // ------------------------------
  debug = true,
  nCbo = 1,

  // ------------------------------
  //   PREVIOUS MEMORY PARAMETERS
  // ------------------------------
  pPrevBus = Array(
    Mb4sConfig (
      debug = true,
      readOnly = false,
      nHart = 2,
      nAddrBit = 32,
      useAmo = false,
      nDataByte = 4,

      useDome = true,
      nDome = 2,
      multiDome = true
    ),
    Mb4sConfig (
      debug = true,
      readOnly = true,
      nHart = 2,
      nAddrBit = 32,
      useAmo = false,
      nDataByte = 4,
  
      useDome = true,
      nDome = 2,
      multiDome = true
    )
  ),

  // ------------------------------
  //         DOME PARAMETERS
  // ------------------------------
  multiDome = true,
  nPart = 2,

  // ------------------------------
  //     CONTROLLER PARAMETERS
  // ------------------------------
  nNextDataByte = 8,
  useReqReg = false,
  useAccReg = false,
  useAckReg = false,
  nWriteFifoDepth = 2,
  nNextFifoDepth = 2,
  nNextLatency = 2,

  // ------------------------------
  //     PREFETCHER PARAMETERS
  // ------------------------------
  usePftch = false,
  nPftchEntry = 4,
  nPftchEntryAcc = 1,
  nPftchMemRead = 2,
  nPftchMemWrite = 1,

  // ------------------------------
  //   PHYSICAL MEMORY PARAMETERS
  // ------------------------------
  nMem = 2,
  nMemReadPort = 2,
  nMemWritePort = 1,

  // ------------------------------
  //        CACHE PARAMETERS
  // ------------------------------ 
  slctPolicy = "BitPLRU",
  nSet = 8,
  nLine = 8,
  nData = 8
)
