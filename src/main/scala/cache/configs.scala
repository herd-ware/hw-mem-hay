/*
 * File: configs.scala                                                         *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:39:53 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay.cache

import chisel3._
import scala.math._


object MemConfigBase extends MemConfig (
  nLogRead = 2,
  nLogWrite = 1,
  nPhyRead = 2,
  nPhyWrite = 1,

  nData = 128,
  nDataByte = 8
)

object CacheConfigBase extends CacheConfig (
  debug = true,
  nHart = 1,
  nCbo = 1,
  
  useDome = true,
  nDome = 2,
  multiDome = false,
  nPart = 2,

  nAccess = 2,
  nReadPort = 2,
  nWritePort = 2,
  nMem = 2,
  nMemReadPort = 2,
  nMemWritePort = 1,
  slctPolicy = "BitPLRU",
  nPendingAcc = 2,

  nData = 4,
  nDataByte = 4,
  nLine = 4,
  nSet = 4,
  nTagBit = 24
)
