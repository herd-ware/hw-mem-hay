/*
 * File: req.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-03-02 01:45:29 pm                                       *
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

import herd.common.gen._
import herd.common.tools._
import herd.common.field._
import herd.common.mem.mb4s._
import herd.mem.hay.common._


class ReqStage(p: PrevUnitParams) extends Module {
  val io = IO(new Bundle {
    val b_field = if (p.useField) Some(Vec(p.nField, new FieldIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useField, p.nField, p.nTagBit, p.nSet)))
    
    val i_slct = if (p.useFieldSlct) Some(Input(new SlctBus(p.nField, p.nPart, 1))) else None
    val b_port = Flipped(new Mb4sReqIO(p.pPrevBus))
    val b_zero = if (!p.readOnly) Some(Flipped(Vec(p.nFieldTag, new ZeroIO(p)))) else None

    val b_dep_back = Vec(p.nFieldSlct, new DependStageIO(p))
    val b_dep_bus = Vec(p.nFieldSlct, new DependStageIO(p))
    val b_dep_reg = if (p.useReqReg) Some(Vec(p.nFieldSlct, new DependStageIO(p))) else None

    val o_slct = if (p.useFieldSlct) Some(Output(new SlctBus(p.nField, p.nPart, 1))) else None    
    val b_out = new GenSRVIO(p, new PrevUnitCtrlBus(p), UInt(0.W))
  })

  val m_out = if (p.useReqReg) Some(Module(new GenDReg(p, new PrevUnitCtrlBus(p), UInt(0.W), false, false, true))) else None
  val m_mux = Module(new GenSMux(p, new PrevUnitCtrlBus(p), UInt(0.W)))

  // ******************************
  //            STATUS
  // ******************************
  val r_new_no = RegInit(VecInit(Seq.fill(p.nFieldSlct)(false.B)))

  val w_wait_back = Wire(Vec(p.nFieldSlct, Bool()))  
  val w_wait_reg = Wire(Vec(p.nFieldSlct, Bool()))
  val w_lock = Wire(Vec(p.nFieldSlct, Bool()))

  // ******************************
  //        FIELD INTERFACE
  // ******************************
  val r_slct_out = Reg(new SlctBus(p.nField, p.nPart, 1))
  val r_port_field = Reg(UInt(log2Ceil(p.nFieldTag).W))

  val w_slct_port = Wire(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_bus = Wire(new SlctBus(p.nField, p.nPart, 1))
  val w_slct_out = Wire(new SlctBus(p.nField, p.nPart, 1))

  if (p.useFieldSlct) {
    w_slct_port.field := io.b_port.field.get
    w_slct_port.next := 0.U
    w_slct_port.step := 0.U
    
    w_slct_bus := io.i_slct.get
    if (p.useReqReg) {
      r_slct_out := w_slct_bus
      w_slct_out := r_slct_out
    } else {
      w_slct_out := w_slct_bus
    }
    io.o_slct.get := w_slct_out
  } else {
    w_slct_port.field := 0.U
    w_slct_port.next := 0.U
    w_slct_port.step := 0.U
    w_slct_bus := w_slct_port
    w_slct_out := w_slct_port   
  }

  if (p.useFieldTag) r_port_field := io.b_port.field.get

  // ******************************
  //          BACK REGISTER
  // ******************************
  val m_back = Module(new GenDReg(p, new PrevUnitCtrlBus(p), UInt(0.W), false, false, false))
  val w_bus = Wire(new GenDVBus(p, new PrevUnitCtrlBus(p), UInt(0.W)))

  // ------------------------------
  //             WAIT
  // ------------------------------  
  for (fs <- 0 until p.nFieldSlct) {
    r_new_no(fs) := false.B
  }

  if (!p.readOnly) {
    if (p.useFieldSlct) {
      for (fs <- 0 until p.nFieldSlct) {
        r_new_no(fs) := io.b_zero.get(fs).valid
      }
    } else if (p.useField) {
      for (f <- 0 until p.nFieldTag) {
        when (f.U === r_port_field) {
          r_new_no(0) := io.b_zero.get(f).valid
        }
      }
    } else {
      r_new_no(0) := io.b_zero.get(0).valid
    }
  }

  // ------------------------------
  //            DEPEND
  // ------------------------------  
  for (fs <- 0 until p.nFieldSlct) {
    io.b_dep_back(fs).reg := DontCare
    io.b_dep_back(fs).reg.valid := m_back.io.o_val.valid(fs) | r_new_no(fs)
  }

  if (p.useFieldSlct) {
    for (fs <- 0 until p.nFieldSlct) {
      io.b_port.ready(fs) := ~r_new_no(fs) & ~io.b_dep_back(fs).lock & ~m_back.io.o_val.valid(fs)
      w_wait_back(fs) := r_new_no(fs) | io.b_dep_back(fs).lock

      io.b_dep_back(fs).reg.field.get := fs.U
    }
  } else {
    io.b_port.ready(0) := ~r_new_no(0) & ~io.b_dep_back(0).lock & ~m_back.io.o_val.valid(0)
    w_wait_back(0) := r_new_no(0) | io.b_dep_back(0).lock

    if (p.useField) io.b_dep_back(0).reg.field.get := r_port_field     
  }

  // ------------------------------
  //        UPDATE REGISTERS
  // ------------------------------
  for (fs <- 0 until p.nFieldSlct) {
    m_back.io.i_flush(fs) := false.B
  }

  if (p.useFieldSlct) {
    for (fs <- 0 until p.nFieldSlct) {
      m_back.io.b_din.valid(fs) := io.b_port.valid & ~w_wait_back(fs) & (w_lock(fs) | (io.b_port.field.get =/= w_slct_bus.field))
      m_back.io.b_din.ctrl.get(fs).setPort(io.b_port.ctrl)

      m_back.io.b_dout.ready(fs) := ~w_lock(fs)
    }
  } else {
    m_back.io.b_din.valid(0) := io.b_port.valid & ~w_wait_back(0) & w_lock(0)
    if (p.useFieldTag) m_back.io.b_din.field.get := io.b_port.field.get
    m_back.io.b_din.ctrl.get(0).setPort(io.b_port.ctrl)

    m_back.io.b_dout.ready(0) := ~w_lock(0)
  }

  // ******************************
  //             BUS
  // ******************************  
  // ------------------------------
  //             MUX
  // ------------------------------  
  if (!p.readOnly) {
    for (f <- 0 until p.nFieldTag) {
      io.b_zero.get(f).ready := false.B
    }
  }

  if (p.useFieldSlct) {
    for (fs <- 0 until p.nFieldSlct) {
      when (m_back.io.b_dout.valid(fs)) {
        w_bus.valid(fs) := true.B
        w_bus.ctrl.get(fs) := m_back.io.b_dout.ctrl.get(fs)
      }.otherwise {
        w_bus.valid(fs) := io.b_port.valid & ~w_wait_back(fs) & (w_slct_bus.field === io.b_port.field.get)
        w_bus.ctrl.get(fs).setPort(io.b_port.ctrl)

        if (!p.readOnly) {
          when (io.b_zero.get(fs).valid) {
            w_bus.valid(fs) := true.B
            w_bus.ctrl.get(fs).info.hart := io.b_zero.get(fs).hart
            w_bus.ctrl.get(fs).info.zero := true.B
            w_bus.ctrl.get(fs).op.op := OP.W
            w_bus.ctrl.get(fs).op.size := SIZE.toSize(p.nDataByte).U
            w_bus.ctrl.get(fs).op.mask := SIZE.toMask(p.nDataByte, SIZE.toSize(p.nDataByte).U)
            w_bus.ctrl.get(fs).addr := io.b_zero.get(fs).addr
          }          
        }
      }
    }
  } else {
    w_bus.valid(0) := m_back.io.b_dout.valid(0)
    if (p.useFieldTag) w_bus.field.get := m_back.io.b_dout.field.get
    w_bus.ctrl.get(0) := m_back.io.b_dout.ctrl.get(0)

    when (~m_back.io.b_dout.valid(0)) {
      w_bus.valid(0) := io.b_port.valid & ~w_wait_back(0)
      if (p.useFieldTag) w_bus.field.get := io.b_port.field.get
      w_bus.ctrl.get(0).setPort(io.b_port.ctrl)

      if (!p.readOnly) {
        if (p.useField) {
          for (f <- 0 until p.nFieldTag) {
            when (io.b_zero.get(f).valid & (f.U === r_port_field)) {
              w_bus.valid(0) := true.B
              w_bus.ctrl.get(0).info.hart := io.b_zero.get(f).hart
              w_bus.ctrl.get(0).info.zero := true.B
              w_bus.ctrl.get(0).op.op := OP.W
              w_bus.ctrl.get(0).op.size := SIZE.toSize(p.nDataByte).U
              w_bus.ctrl.get(0).op.mask := SIZE.toMask(p.nDataByte, SIZE.toSize(p.nDataByte).U)
              w_bus.ctrl.get(0).addr := io.b_zero.get(f).addr
            }
          }
        } else {
          when (io.b_zero.get(0).valid) {
            w_bus.valid(0) := true.B
            w_bus.ctrl.get(0).info.hart := io.b_zero.get(0).hart
            w_bus.ctrl.get(0).info.zero := true.B
            w_bus.ctrl.get(0).op.op := OP.W
            w_bus.ctrl.get(0).op.size := SIZE.toSize(p.nDataByte).U
            w_bus.ctrl.get(0).op.mask := SIZE.toMask(p.nDataByte, SIZE.toSize(p.nDataByte).U)
            w_bus.ctrl.get(0).addr := io.b_zero.get(0).addr
          }
        }
      }      
    }
  }

  // ------------------------------
  //            DEPEND
  // ------------------------------  
  for (fs <- 0 until p.nFieldSlct) {
    io.b_dep_bus(fs).reg.lock := (fs.U =/= w_slct_out.field) | ~io.b_out.ready
    io.b_dep_bus(fs).reg.valid := w_bus.valid(fs) 
    io.b_dep_bus(fs).reg.hart := w_bus.ctrl.get(fs).info.hart
    io.b_dep_bus(fs).reg.rw := w_bus.ctrl.get(fs).op.wa
    io.b_dep_bus(fs).reg.addr := w_bus.ctrl.get(fs).addr
    if (p.useFieldSlct) {
      io.b_dep_bus(fs).reg.field.get := fs.U
    } else if (p.useField) {
      io.b_dep_bus(0).reg.field.get := w_bus.field.get
    }

    if (p.useReqReg) {
      io.b_dep_bus(fs).reg.lock := ~m_out.get.io.b_din.ready(fs)

      io.b_dep_reg.get(fs).reg.lock := (fs.U =/= w_slct_out.field) | ~io.b_out.ready
      io.b_dep_reg.get(fs).reg.valid := m_out.get.io.o_val.valid(fs)  
      io.b_dep_reg.get(fs).reg.hart := m_out.get.io.o_val.ctrl.get(fs).info.hart
      io.b_dep_reg.get(fs).reg.addr := m_out.get.io.o_val.ctrl.get(fs).addr
      io.b_dep_reg.get(fs).reg.rw := m_out.get.io.o_val.ctrl.get(fs).op.wa
      if (p.useFieldSlct) {
        io.b_dep_reg.get(fs).reg.field.get := fs.U
      } else if (p.useField) {
        io.b_dep_reg.get(0).reg.field.get := m_out.get.io.o_val.field.get
      }
    }
  }

  // ******************************
  //           REGISTERS
  // ******************************
  for (fs <- 0 until p.nFieldSlct) {
    w_lock(fs) := io.b_dep_bus(fs).lock | w_wait_reg(fs)

    if (p.useReqReg) {
      w_wait_reg(fs) := ~m_out.get.io.b_din.ready(fs)

      for (fs <- 0 until p.nFieldSlct) {
        m_out.get.io.i_flush(fs) := false.B        
      }

      m_out.get.io.b_din.valid(fs) := w_bus.valid(fs) & ~io.b_dep_bus(fs).lock
      if (p.useFieldTag) m_out.get.io.b_din.field.get := w_bus.field.get  
      m_out.get.io.b_din.ctrl.get(fs) := w_bus.ctrl.get(fs)

      m_mux.io.b_din <> m_out.get.io.b_dout
      
    } else {
      w_wait_reg(fs) := ~m_mux.io.b_din.ready(fs)

      m_mux.io.b_din.valid(fs) := w_bus.valid(fs) & ~io.b_dep_bus(fs).lock
      if (p.useFieldTag) m_mux.io.b_din.field.get := w_bus.field.get 
      m_mux.io.b_din.ctrl.get(fs) := w_bus.ctrl.get(fs)
    }  
  }

  if (p.useFieldSlct) m_mux.io.i_slct.get := w_slct_out
  m_mux.io.b_sout <> io.b_out
  if (p.useReqReg) {
    for (fs <- 0 until p.nFieldSlct) {
      when (fs.U === w_slct_out.field) {
        m_mux.io.b_sout.ready := io.b_out.ready & ~io.b_dep_reg.get(fs).lock
        io.b_out.valid := m_mux.io.b_sout.valid & ~io.b_dep_reg.get(fs).lock
      }
    }
  }

  // ******************************
  //             FLUSH
  // ******************************
  // ------------------------------
  //             FREE
  // ------------------------------
  val w_r_free = Wire(Vec(p.nFieldSlct, Bool()))

  for (fs <- 0 until p.nFieldSlct) {
    if (p.useReqReg) {
      w_r_free(fs) := ~m_out.get.io.o_val.valid(fs)
    } else {
      w_r_free(fs) := true.B
    }
  }  
  
  // ------------------------------
  //             WAIT
  // ------------------------------
  for (f <- 0 until p.nCbo) {
    when (io.b_cbo(f).valid & io.b_cbo(f).inv) {
      if (p.useFieldSlct) {
        r_new_no(io.b_cbo(f).field.get) := true.B        
      } else if (p.useFieldTag) {
        when (io.b_cbo(f).field.get === r_port_field) {
          r_new_no(0) := true.B
        }
      } else {
        r_new_no(0) := true.B
      }      
    }
  }

  // ******************************
  //            CBO
  // ******************************
  for (c <- 0 until p.nCbo) {
    io.b_cbo(c).ready := io.b_cbo(c).valid

    when (io.b_cbo(c).valid) {
      when (io.b_cbo(c).inv) {
        if (p.useFieldSlct) {
          for (f <- 0 until p.nField) {
            when (f.U === io.b_cbo(c).field.get) {
              io.b_cbo(c).ready := ~m_back.io.o_val.valid(f) & w_r_free(f)
            }
          }
        } else if (p.useFieldTag) {
          when (io.b_cbo(c).field.get === r_port_field) {
            io.b_cbo(c).ready := ~m_back.io.o_val.valid(0) & w_r_free(0)
          }
        } else {
          io.b_cbo(c).ready := ~m_back.io.o_val.valid(0) & w_r_free(0)
        }
      }
    }       
  }

  // ******************************
  //            FIELD
  // ******************************
  for (f <- 0 until p.nField) {
    if (p.useFieldSlct) {
      when (io.b_field.get(f).valid & io.b_field.get(f).flush) {
        r_new_no(f) := true.B
      }

      io.b_field.get(f).free := ~m_back.io.o_val.valid(f) & w_r_free(f)
    } else if (p.useFieldTag) {
      when (io.b_field.get(f).valid & io.b_field.get(f).flush & (f.U === r_port_field)) {
        r_new_no(0) := true.B
      }

      io.b_field.get(f).free := (f.U =/= r_port_field) | (~m_back.io.o_val.valid(0) & w_r_free(0))
    }      
  } 

  // ******************************
  //             DEBUG
  // ******************************
  if (p.debug) {
    // ------------------------------
    //            SIGNALS
    // ------------------------------
    dontTouch(w_wait_reg)
    dontTouch(w_lock)

    // ------------------------------
    //         DATA FOOTPRINT
    // ------------------------------

    // ------------------------------
    //         MEMORY TRACKER
    // ------------------------------
    if (!p.readOnly) {
      if (p.useFieldSlct) {
        for (fs <- 0 until p.nFieldSlct) {
          when (~m_back.io.b_dout.valid(fs) & io.b_zero.get(fs).valid) {
            w_bus.ctrl.get(fs).mtd.get.addr := io.b_zero.get(fs).addr.toFull()
          }
        }
      } else if (p.useField) {
        for (f <- 0 until p.nFieldTag) {
          when (~m_back.io.b_dout.valid(0) & io.b_zero.get(f).valid & (f.U === r_port_field)) {
            w_bus.ctrl.get(0).mtd.get.addr := io.b_zero.get(f).addr.toFull()
          }
        }
      } else {
        when (~m_back.io.b_dout.valid(0) & io.b_zero.get(0).valid) {
          w_bus.ctrl.get(0).mtd.get.addr := io.b_zero.get(0).addr.toFull()
        }
      }
    }
  }
}

class ReqDepend(p: PrevParams) extends Module {
  // Number of parallel registers by adding each PrevCtrl
  def nReg: Int = {
    var r: Int = 0
    
    for (np <- 0 until p.nPrevPort) {
      r = r + p.pPrev(np).nFieldSlct
    }

    return r
  }

  // Keep associated register number and original PrevCtrl number
  var nRegPrev = new Array[Int](nReg)
  for (np <- 0 until p.nPrevPort) {
    var r: Int = 0
    
    for (nds <- 0 until p.pPrev(np).nFieldSlct) {
      nRegPrev(r + nds) = np
    }

    r = r + p.pPrev(np).nFieldSlct
  }


  val io = IO(new Bundle {
    val b_back = Vec(nReg, Flipped(new DependStageIO(p)))
    val b_bus = Vec(nReg, Flipped(new DependStageIO(p)))
    val b_reg = if (p.useReqReg) Some(Vec(nReg, Flipped(new DependStageIO(p)))) else None
    val i_acc = Input(Vec(nReg, new DependRegBus(p)))
  })

  dontTouch(io.b_back)
  dontTouch(io.b_bus)
  dontTouch(io.i_acc)
  if (p.useReqReg) dontTouch(io.b_reg.get)

  // ******************************
  //            DEFAULT 
  // ******************************
  for (r <- 0 until nReg) {
    io.b_back(r).lock := false.B
    io.b_bus(r).lock := false.B
    if (p.useReqReg) io.b_reg.get(r).lock := false.B
  }

  // ******************************
  //             BACK 
  // ******************************
  for (r0 <- 0 until nReg) {
    for (r1 <- 0 until nReg) {
      val w_field_back = Wire(Bool())

      if (p.useField) {
        w_field_back := (io.b_back(r0).reg.field.get === io.b_back(r1).reg.field.get)
      } else {
        w_field_back := true.B
      }

      // Lock new request
      if (r0 != r1) {
        when (io.b_back(r0).reg.valid) {          
          when (w_field_back) {
            io.b_back(r1).lock := true.B
          }
        }
      }
    }
  }

  // ******************************
  //          VERTICAL DEP 
  // ******************************  
  for (r0 <- 0 until nReg) {
    for (r1 <- (r0 + 1) until nReg) {
      val w_field_bus = Wire(Bool())
      val w_field_reg = Wire(Bool())

      if (p.useField) {
        if (p.useReqReg) {
          w_field_bus := (io.b_bus(r0).reg.field.get === io.b_reg.get(r1).reg.field.get)
          w_field_reg := (io.b_reg.get(r0).reg.field.get === io.i_acc(r1).field.get)
        } else {
          w_field_bus := (io.b_bus(r0).reg.field.get === io.i_acc(r1).field.get)
          w_field_reg := true.B
        }        
      } else {
        w_field_bus := true.B
        w_field_reg := true.B
      }

      if (p.useReqReg) {
        when (io.b_reg.get(r1).reg.valid & io.b_reg.get(r1).reg.lock & w_field_reg) {
          io.b_bus(r0).lock := true.B
        }
        when (io.i_acc(r1).valid & io.i_acc(r1).lock & w_field_reg) {
          io.b_reg.get(r0).lock := true.B
        }
      } else {
        when (io.i_acc(r1).valid & io.i_acc(r1).lock & w_field_bus) {
          io.b_bus(r0).lock := true.B
        }
      }      
    }
  }  

  // ******************************
  //         HORIZONTAL DEP 
  // ******************************  
  for (r0 <- 0 until nReg) {
    for (r1 <- 0 until r0) {
      val w_field_bus = Wire(Bool())

      if (p.useField) {
        w_field_bus := (io.b_bus(r0).reg.field.get === io.b_bus(r1).reg.field.get)
      } else {
        w_field_bus := true.B
      }

      if (nRegPrev(r0) != nRegPrev(r1)) {
        when (io.b_bus(r1).reg.valid & io.b_bus(r1).reg.lock & w_field_bus) {
          io.b_bus(r0).lock := true.B
        }
      }

      if (p.useReqReg) {
        val w_field_reg = Wire(Bool())

        if (p.useField) {
          w_field_reg := (io.b_reg.get(r0).reg.field.get === io.b_reg.get(r1).reg.field.get)
        } else {
          w_field_reg := true.B
        }

        when (io.b_reg.get(r1).reg.valid & io.b_reg.get(r1).reg.lock & w_field_reg) {
          io.b_reg.get(r0).lock := true.B
        }
      }
    }
  }
}

object ReqStage extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new ReqStage(PrevUnitConfigBase), args)
}