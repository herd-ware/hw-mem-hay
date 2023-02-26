/*
 * File: params.scala                                                          *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:40:27 pm                                       *
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
import scala.math._

import herd.common.mem.mb4s._
import herd.mem.hay.cache._


trait PftchParams extends CacheParams {
  def nPrevPort: Int

  def debug: Boolean
  def nAddrBit: Int
  def readOnly: Boolean
  def nHart: Int
  def nCbo: Int

  def useDome: Boolean
  def nDome: Int
  def nPart: Int
  def multiDome: Boolean

  def nNextDataByte: Int
  def nNextDataBit: Int = nNextDataByte * 8

  def usePftch: Boolean
  def nPftchEntry: Int
  def nPftchEntryAcc: Int
  def nPftchMemRead: Int
  def nPftchMemWrite: Int
  def nPftchTagBit: Int = nTagBit + log2Ceil(nSet)
  def nPftchOp: Int = {
    if (!readOnly) {
      return nPrevPort * 2
    } else {
      return nPrevPort
    }
  }
  def nPftchFifoDepth: Int

  def nAccess: Int = 1
  def nReadPort: Int = 1
  def nWritePort: Int = 1
  def nPendingAcc: Int = 1

  def nMem: Int
  def nMemReadPort: Int
  def nMemWritePort: Int

  def slctPolicy: String
  def nSet: Int
  def nLine: Int
  def nData: Int
  def nDataByte: Int = nNextDataByte
  def nTagBit: Int = nAddrBit - log2Ceil(nSet) - log2Ceil(nLineByte)

  def pPftchMem: MemParams = new MemConfig (
    nLogRead = (nPrevPort + 1),
    nLogWrite = if (!readOnly) {
      (nPrevPort + 1) 
    } else {
      1
    },
    nPhyRead = nPftchMemRead,
    nPhyWrite = nPftchMemWrite,
    nData = (nPftchEntry * nData),
    nDataByte = nDataByte
  )

  def pNextPort: Mb4sParams = new Mb4sConfig (
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
}

case class PftchConfig (
  nPrevPort: Int,

  debug: Boolean,
  nAddrBit: Int,
  readOnly: Boolean,
  nHart: Int,
  nCbo: Int,

  useDome: Boolean,
  nDome: Int,
  multiDome: Boolean,
  nPart: Int,

  nNextDataByte: Int,

  usePftch: Boolean,
  nPftchEntry: Int,
  nPftchEntryAcc: Int,
  nPftchMemRead: Int,
  nPftchMemWrite: Int,
  nPftchFifoDepth: Int,

  nMem: Int,
  nMemReadPort: Int,
  nMemWritePort: Int,

  slctPolicy: String,
  nSet: Int,
  nLine: Int,
  nData: Int
) extends PftchParams