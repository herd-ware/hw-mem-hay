/*
 * File: params.scala                                                          *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:00 pm                                       *
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
import scala.math._

import herd.common.gen._


// ******************************
//            MEMORY
// ******************************
trait MemParams {
  def nLogRead: Int
  def nLogWrite: Int
  def nPhyRead: Int
  def nPhyWrite: Int

  def nData: Int
  def nDataByte: Int
}

case class MemConfig (
  nLogRead: Int,
  nLogWrite: Int,
  nPhyRead: Int,
  nPhyWrite: Int,

  nData: Int,
  nDataByte: Int
) extends MemParams

// ******************************
//             CACHE
// ******************************
trait CacheParams extends GenParams {
  def debug: Boolean
  def nHart: Int
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
  def nDataBit: Int = nDataByte * 8
  def nLine: Int
  def nLineByte: Int = (nData * nDataByte)
  def nSet: Int
  def nTagBit: Int

  def nMem: Int
  def nMemData: Int = ((nSet * nLine * nData) / nMem).toInt
  def nMemReadPort: Int
  def nMemWritePort: Int
  def pMem: MemParams = new MemConfig (
    nLogRead = nReadPort,
    nLogWrite = nWritePort,
    nPhyRead = nMemReadPort,
    nPhyWrite = nMemWritePort,
    nData = nMemData,
    nDataByte = nDataByte
  )
}

case class CacheConfig (
  debug: Boolean,
  nHart: Int,
  nCbo: Int,

  useField: Boolean,
  nField: Int,
  multiField: Boolean,
  nPart: Int,

  nAccess: Int,
  nReadPort: Int,
  nWritePort: Int,
  nMem: Int,
  nMemReadPort: Int,
  nMemWritePort: Int,
  nPendingAcc: Int,

  slctPolicy: String,
  nData: Int,
  nDataByte: Int,
  nLine: Int,
  nSet: Int,
  nTagBit: Int
) extends CacheParams
