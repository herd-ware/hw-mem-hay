/*
 * File: configs.scala                                                         *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:37 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay.pftch

import chisel3._
import scala.math._

import herd.common.mem.mb4s._


object PftchConfigBase extends PftchConfig (
  nPrevPort = 2,

  debug = false,
  nAddrBit = 32,
  readOnly = false,
  nHart = 2,
  nCbo = 1,

  useDome = true,
  nDome = 2,
  multiDome = true,
  nPart = 2,

  nNextDataByte = 8,

  usePftch = true,
  nPftchEntry = 4,
  nPftchEntryAcc = 2,
  nPftchMemRead = 2,
  nPftchMemWrite = 1,
  nPftchFifoDepth = 2,

  nMem = 2,
  nMemReadPort = 2,
  nMemWritePort = 1,

  slctPolicy = "BitPLRU",
  nSet = 4,
  nLine = 4,
  nData = 4
)
