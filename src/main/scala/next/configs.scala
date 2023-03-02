/*
 * File: configs.scala                                                         *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:08 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay.next

import chisel3._
import scala.math._

import herd.common.mem.mb4s._


object NextConfigBase extends NextConfig (
  nPrevPort = 2,

  debug = false,
  nAddrBit = 32,
  readOnly = false,
  nHart = 2,
  nCbo = 1,

  useField = true,
  nField = 2,
  multiField = true,
  nPart = 2,

  nPrevDataByte = 4,
  nNextDataByte = 8,
  nNextFifoDepth = 2,
  nNextLatency = 2,

  nMem = 2,
  nMemReadPort = 2,
  nMemWritePort = 1,

  slctPolicy = "BitPLRU",
  nSet = 4,
  nLine = 4,
  nData = 4
)
