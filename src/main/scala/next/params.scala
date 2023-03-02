/*
 * File: params.scala                                                          *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:21 pm                                       *
 * Modified By: Mathieu Escouteloup                                            *
 * -----                                                                       *
 * License: See LICENSE.md                                                     *
 * Copyright (c) 2023 HerdWare                                                *
 * -----                                                                       *
 * Description:                                                                *
 */


package herd.mem.hay.next

import chisel3._
import chisel3.util._
import scala.math._

import herd.common.mem.mb4s._
import herd.mem.hay.cache._


trait NextParams extends CacheParams {
  def nPrevPort: Int

  def debug: Boolean
  def nAddrBit: Int
  def readOnly: Boolean
  def nHart: Int
  def nCbo: Int

  def useField: Boolean
  def nField: Int
  def nPart: Int
  def multiField: Boolean

  def nPrevDataByte: Int
  def nPrevDataBit: Int = nPrevDataByte * 8
  def nNextDataByte: Int
  def nNextDataBit: Int = nNextDataByte * 8
  def nNextFifoDepth: Int
  def nRepCycle: Int = (nDataByte * nData) / nNextDataByte
  def nNextLatency: Int

  def nAccess: Int = 1
  def nReadPort: Int = 0
  def nWritePort: Int = 2
  def nPendingAcc: Int = 1

  def nMem: Int
  def nMemReadPort: Int
  def nMemWritePort: Int

  def useRsv: Boolean = false
  def slctPolicy: String
  def nSet: Int
  def nLine: Int
  def nData: Int
  def nDataByte: Int = max(nPrevDataByte, nNextDataByte)
  def nTagBit: Int = nAddrBit - log2Ceil(nSet) - log2Ceil(nLineByte)

  def pNextBus: Mb4sParams = new Mb4sConfig (
    debug = debug,
    readOnly = readOnly,
    nHart = nHart,
    nAddrBit = nAddrBit,
    useAmo = false,
    nDataByte = nNextDataByte,
    useField =  useField,
    nField = nField,
    multiField = multiField
  )
}

case class NextConfig (
  nPrevPort: Int,

  debug: Boolean,
  nAddrBit: Int,
  readOnly: Boolean,
  nHart: Int,
  nCbo: Int,

  useField: Boolean,
  nField: Int,
  multiField: Boolean,
  nPart: Int,

  nPrevDataByte: Int,
  nNextDataByte: Int,
  nNextFifoDepth: Int,
  nNextLatency: Int,

  nMem: Int,
  nMemReadPort: Int,
  nMemWritePort: Int,

  slctPolicy: String,
  nSet: Int,
  nLine: Int,
  nData: Int
) extends NextParams