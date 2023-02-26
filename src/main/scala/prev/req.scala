/*
 * File: req.scala                                                             *
 * Created Date: 2023-02-25 04:11:31 pm                                        *
 * Author: Mathieu Escouteloup                                                 *
 * -----                                                                       *
 * Last Modified: 2023-02-25 09:41:08 pm                                       *
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
import herd.common.dome._
import herd.common.mem.mb4s._
import herd.mem.hay.common._


class ReqStage(p: PrevUnitParams) extends Module {
  val io = IO(new Bundle {
    val b_dome = if (p.useDome) Some(Vec(p.nDome, new DomeIO(p.nAddrBit, p.nDataBit))) else None
    val b_cbo = Vec(p.nCbo, Flipped(new CboIO(p.nHart, p.useDome, p.nDome, p.nTagBit, p.nSet)))
    
    val i_slct = if (p.useDomeSlct) Some(Input(new SlctBus(p.nDome, p.nPart, 1))) else None
    val b_port = Flipped(new Mb4sReqIO(p.pPrevBus))
    val b_zero = if (!p.readOnly) Some(Flipped(Vec(p.nDomeTag, new ZeroIO(p)))) else None

    val b_dep_back = Vec(p.nDomeSlct, new DependStageIO(p))
    val b_dep_bus = Vec(p.nDomeSlct, new DependStageIO(p))
    val b_dep_reg = if (p.useReqReg) Some(Vec(p.nDomeSlct, new DependStageIO(p))) else None

    val o_slct = if (p.useDomeSlct) Some(Output(new SlctBus(p.nDome, p.nPart, 1))) else None    
    val b_out = new GenSRVIO(p, new PrevUnitCtrlBus(p), UInt(0.W))
  })

  val m_out = if (p.useReqReg) Some(Module(new GenDReg(p, new PrevUnitCtrlBus(p), UInt(0.W), false, false, true))) else None
  val m_mux = Module(new GenSMux(p, new PrevUnitCtrlBus(p), UInt(0.W)))

  // ******************************
  //            STATUS
  // ******************************
  val r_new_no = RegInit(VecInit(Seq.fill(p.nDomeSlct)(false.B)))

  val w_wait_back = Wire(Vec(p.nDomeSlct, Bool()))  
  val w_wait_reg = Wire(Vec(p.nDomeSlct, Bool()))
  val w_lock = Wire(Vec(p.nDomeSlct, Bool()))

  // ******************************
  //         DOME INTERFACE
  // ******************************
  val r_slct_out = Reg(new SlctBus(p.nDome, p.nPart, 1))
  val r_port_dome = Reg(UInt(log2Ceil(p.nDomeTag).W))

  val w_slct_port = Wire(new SlctBus(p.nDome, p.nPart, 1))
  val w_slct_bus = Wire(new SlctBus(p.nDome, p.nPart, 1))
  val w_slct_out = Wire(new SlctBus(p.nDome, p.nPart, 1))

  if (p.useDomeSlct) {
    w_slct_port.dome := io.b_port.dome.get
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
    w_slct_port.dome := 0.U
    w_slct_port.next := 0.U
    w_slct_port.step := 0.U
    w_slct_bus := w_slct_port
    w_slct_out := w_slct_port   
  }

  if (p.useDomeTag) r_port_dome := io.b_port.dome.get

  // ******************************
  //          BACK REGISTER
  // ******************************
  val m_back = Module(new GenDReg(p, new PrevUnitCtrlBus(p), UInt(0.W), false, false, false))
  val w_bus = Wire(new GenDVBus(p, new PrevUnitCtrlBus(p), UInt(0.W)))

  // ------------------------------
  //             WAIT
  // ------------------------------  
  for (ds <- 0 until p.nDomeSlct) {
    r_new_no(ds) := false.B
  }

  if (!p.readOnly) {
    if (p.useDomeSlct) {
      for (ds <- 0 until p.nDomeSlct) {
        r_new_no(ds) := io.b_zero.get(ds).valid
      }
    } else if (p.useDome) {
      for (d <- 0 until p.nDomeTag) {
        when (d.U === r_port_dome) {
          r_new_no(0) := io.b_zero.get(d).valid
        }
      }
    } else {
      r_new_no(0) := io.b_zero.get(0).valid
    }
  }

  // ------------------------------
  //            DEPEND
  // ------------------------------  
  for (ds <- 0 until p.nDomeSlct) {
    io.b_dep_back(ds).reg := DontCare
    io.b_dep_back(ds).reg.valid := m_back.io.o_val.valid(ds) | r_new_no(ds)
  }

  if (p.useDomeSlct) {
    for (ds <- 0 until p.nDomeSlct) {
      io.b_port.ready(ds) := ~r_new_no(ds) & ~io.b_dep_back(ds).lock & ~m_back.io.o_val.valid(ds)
      w_wait_back(ds) := r_new_no(ds) | io.b_dep_back(ds).lock

      io.b_dep_back(ds).reg.dome.get := ds.U
    }
  } else {
    io.b_port.ready(0) := ~r_new_no(0) & ~io.b_dep_back(0).lock & ~m_back.io.o_val.valid(0)
    w_wait_back(0) := r_new_no(0) | io.b_dep_back(0).lock

    if (p.useDome) io.b_dep_back(0).reg.dome.get := r_port_dome     
  }

  // ------------------------------
  //        UPDATE REGISTERS
  // ------------------------------
  for (ds <- 0 until p.nDomeSlct) {
    m_back.io.i_flush(ds) := false.B
  }

  if (p.useDomeSlct) {
    for (ds <- 0 until p.nDomeSlct) {
      m_back.io.b_din.valid(ds) := io.b_port.valid & ~w_wait_back(ds) & (w_lock(ds) | (io.b_port.dome.get =/= w_slct_bus.dome))
      m_back.io.b_din.ctrl.get(ds).setPort(io.b_port.ctrl)

      m_back.io.b_dout.ready(ds) := ~w_lock(ds)
    }
  } else {
    m_back.io.b_din.valid(0) := io.b_port.valid & ~w_wait_back(0) & w_lock(0)
    if (p.useDomeTag) m_back.io.b_din.dome.get := io.b_port.dome.get
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
    for (d <- 0 until p.nDomeTag) {
      io.b_zero.get(d).ready := false.B
    }
  }

  if (p.useDomeSlct) {
    for (ds <- 0 until p.nDomeSlct) {
      when (m_back.io.b_dout.valid(ds)) {
        w_bus.valid(ds) := true.B
        w_bus.ctrl.get(ds) := m_back.io.b_dout.ctrl.get(ds)
      }.otherwise {
        w_bus.valid(ds) := io.b_port.valid & ~w_wait_back(ds) & (w_slct_bus.dome === io.b_port.dome.get)
        w_bus.ctrl.get(ds).setPort(io.b_port.ctrl)

        if (!p.readOnly) {
          when (io.b_zero.get(ds).valid) {
            w_bus.valid(ds) := true.B
            w_bus.ctrl.get(ds).info.hart := io.b_zero.get(ds).hart
            w_bus.ctrl.get(ds).info.zero := true.B
            w_bus.ctrl.get(ds).op.op := OP.W
            w_bus.ctrl.get(ds).op.size := SIZE.toSize(p.nDataByte).U
            w_bus.ctrl.get(ds).op.mask := SIZE.toMask(p.nDataByte, SIZE.toSize(p.nDataByte).U)
            w_bus.ctrl.get(ds).addr := io.b_zero.get(ds).addr
          }          
        }
      }
    }
  } else {
    w_bus.valid(0) := m_back.io.b_dout.valid(0)
    if (p.useDomeTag) w_bus.dome.get := m_back.io.b_dout.dome.get
    w_bus.ctrl.get(0) := m_back.io.b_dout.ctrl.get(0)

    when (~m_back.io.b_dout.valid(0)) {
      w_bus.valid(0) := io.b_port.valid & ~w_wait_back(0)
      if (p.useDomeTag) w_bus.dome.get := io.b_port.dome.get
      w_bus.ctrl.get(0).setPort(io.b_port.ctrl)

      if (!p.readOnly) {
        if (p.useDome) {
          for (d <- 0 until p.nDomeTag) {
            when (io.b_zero.get(d).valid & (d.U === r_port_dome)) {
              w_bus.valid(0) := true.B
              w_bus.ctrl.get(0).info.hart := io.b_zero.get(d).hart
              w_bus.ctrl.get(0).info.zero := true.B
              w_bus.ctrl.get(0).op.op := OP.W
              w_bus.ctrl.get(0).op.size := SIZE.toSize(p.nDataByte).U
              w_bus.ctrl.get(0).op.mask := SIZE.toMask(p.nDataByte, SIZE.toSize(p.nDataByte).U)
              w_bus.ctrl.get(0).addr := io.b_zero.get(d).addr
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
  for (ds <- 0 until p.nDomeSlct) {
    io.b_dep_bus(ds).reg.lock := (ds.U =/= w_slct_out.dome) | ~io.b_out.ready
    io.b_dep_bus(ds).reg.valid := w_bus.valid(ds) 
    io.b_dep_bus(ds).reg.hart := w_bus.ctrl.get(ds).info.hart
    io.b_dep_bus(ds).reg.rw := w_bus.ctrl.get(ds).op.wa
    io.b_dep_bus(ds).reg.addr := w_bus.ctrl.get(ds).addr
    if (p.useDomeSlct) {
      io.b_dep_bus(ds).reg.dome.get := ds.U
    } else if (p.useDome) {
      io.b_dep_bus(0).reg.dome.get := w_bus.dome.get
    }

    if (p.useReqReg) {
      io.b_dep_bus(ds).reg.lock := ~m_out.get.io.b_din.ready(ds)

      io.b_dep_reg.get(ds).reg.lock := (ds.U =/= w_slct_out.dome) | ~io.b_out.ready
      io.b_dep_reg.get(ds).reg.valid := m_out.get.io.o_val.valid(ds)  
      io.b_dep_reg.get(ds).reg.hart := m_out.get.io.o_val.ctrl.get(ds).info.hart
      io.b_dep_reg.get(ds).reg.addr := m_out.get.io.o_val.ctrl.get(ds).addr
      io.b_dep_reg.get(ds).reg.rw := m_out.get.io.o_val.ctrl.get(ds).op.wa
      if (p.useDomeSlct) {
        io.b_dep_reg.get(ds).reg.dome.get := ds.U
      } else if (p.useDome) {
        io.b_dep_reg.get(0).reg.dome.get := m_out.get.io.o_val.dome.get
      }
    }
  }

  // ******************************
  //           REGISTERS
  // ******************************
  for (ds <- 0 until p.nDomeSlct) {
    w_lock(ds) := io.b_dep_bus(ds).lock | w_wait_reg(ds)

    if (p.useReqReg) {
      w_wait_reg(ds) := ~m_out.get.io.b_din.ready(ds)

      for (ds <- 0 until p.nDomeSlct) {
        m_out.get.io.i_flush(ds) := false.B        
      }

      m_out.get.io.b_din.valid(ds) := w_bus.valid(ds) & ~io.b_dep_bus(ds).lock
      if (p.useDomeTag) m_out.get.io.b_din.dome.get := w_bus.dome.get  
      m_out.get.io.b_din.ctrl.get(ds) := w_bus.ctrl.get(ds)

      m_mux.io.b_din <> m_out.get.io.b_dout
      
    } else {
      w_wait_reg(ds) := ~m_mux.io.b_din.ready(ds)

      m_mux.io.b_din.valid(ds) := w_bus.valid(ds) & ~io.b_dep_bus(ds).lock
      if (p.useDomeTag) m_mux.io.b_din.dome.get := w_bus.dome.get 
      m_mux.io.b_din.ctrl.get(ds) := w_bus.ctrl.get(ds)
    }  
  }

  if (p.useDomeSlct) m_mux.io.i_slct.get := w_slct_out
  m_mux.io.b_sout <> io.b_out
  if (p.useReqReg) {
    for (ds <- 0 until p.nDomeSlct) {
      when (ds.U === w_slct_out.dome) {
        m_mux.io.b_sout.ready := io.b_out.ready & ~io.b_dep_reg.get(ds).lock
        io.b_out.valid := m_mux.io.b_sout.valid & ~io.b_dep_reg.get(ds).lock
      }
    }
  }

  // ******************************
  //             FLUSH
  // ******************************
  // ------------------------------
  //             FREE
  // ------------------------------
  val w_r_free = Wire(Vec(p.nDomeSlct, Bool()))

  for (ds <- 0 until p.nDomeSlct) {
    if (p.useReqReg) {
      w_r_free(ds) := ~m_out.get.io.o_val.valid(ds)
    } else {
      w_r_free(ds) := true.B
    }
  }  
  
  // ------------------------------
  //             WAIT
  // ------------------------------
  for (f <- 0 until p.nCbo) {
    when (io.b_cbo(f).valid & io.b_cbo(f).inv) {
      if (p.useDomeSlct) {
        r_new_no(io.b_cbo(f).dome.get) := true.B        
      } else if (p.useDomeTag) {
        when (io.b_cbo(f).dome.get === r_port_dome) {
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
        if (p.useDomeSlct) {
          for (d <- 0 until p.nDome) {
            when (d.U === io.b_cbo(c).dome.get) {
              io.b_cbo(c).ready := ~m_back.io.o_val.valid(d) & w_r_free(d)
            }
          }
        } else if (p.useDomeTag) {
          when (io.b_cbo(c).dome.get === r_port_dome) {
            io.b_cbo(c).ready := ~m_back.io.o_val.valid(0) & w_r_free(0)
          }
        } else {
          io.b_cbo(c).ready := ~m_back.io.o_val.valid(0) & w_r_free(0)
        }
      }
    }       
  }

  // ******************************
  //             DOME
  // ******************************
  for (d <- 0 until p.nDome) {
    if (p.useDomeSlct) {
      when (io.b_dome.get(d).valid & io.b_dome.get(d).flush) {
        r_new_no(d) := true.B
      }

      io.b_dome.get(d).free := ~m_back.io.o_val.valid(d) & w_r_free(d)
    } else if (p.useDomeTag) {
      when (io.b_dome.get(d).valid & io.b_dome.get(d).flush & (d.U === r_port_dome)) {
        r_new_no(0) := true.B
      }

      io.b_dome.get(d).free := (d.U =/= r_port_dome) | (~m_back.io.o_val.valid(0) & w_r_free(0))
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
      if (p.useDomeSlct) {
        for (ds <- 0 until p.nDomeSlct) {
          when (~m_back.io.b_dout.valid(ds) & io.b_zero.get(ds).valid) {
            w_bus.ctrl.get(ds).mtd.get.addr := io.b_zero.get(ds).addr.toFull()
          }
        }
      } else if (p.useDome) {
        for (d <- 0 until p.nDomeTag) {
          when (~m_back.io.b_dout.valid(0) & io.b_zero.get(d).valid & (d.U === r_port_dome)) {
            w_bus.ctrl.get(0).mtd.get.addr := io.b_zero.get(d).addr.toFull()
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
      r = r + p.pPrev(np).nDomeSlct
    }

    return r
  }

  // Keep associated register number and original PrevCtrl number
  var nRegPrev = new Array[Int](nReg)
  for (np <- 0 until p.nPrevPort) {
    var r: Int = 0
    
    for (nds <- 0 until p.pPrev(np).nDomeSlct) {
      nRegPrev(r + nds) = np
    }

    r = r + p.pPrev(np).nDomeSlct
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
      val w_dome_back = Wire(Bool())

      if (p.useDome) {
        w_dome_back := (io.b_back(r0).reg.dome.get === io.b_back(r1).reg.dome.get)
      } else {
        w_dome_back := true.B
      }

      // Lock new request
      if (r0 != r1) {
        when (io.b_back(r0).reg.valid) {          
          when (w_dome_back) {
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
      val w_dome_bus = Wire(Bool())
      val w_dome_reg = Wire(Bool())

      if (p.useDome) {
        if (p.useReqReg) {
          w_dome_bus := (io.b_bus(r0).reg.dome.get === io.b_reg.get(r1).reg.dome.get)
          w_dome_reg := (io.b_reg.get(r0).reg.dome.get === io.i_acc(r1).dome.get)
        } else {
          w_dome_bus := (io.b_bus(r0).reg.dome.get === io.i_acc(r1).dome.get)
          w_dome_reg := true.B
        }        
      } else {
        w_dome_bus := true.B
        w_dome_reg := true.B
      }

      if (p.useReqReg) {
        when (io.b_reg.get(r1).reg.valid & io.b_reg.get(r1).reg.lock & w_dome_reg) {
          io.b_bus(r0).lock := true.B
        }
        when (io.i_acc(r1).valid & io.i_acc(r1).lock & w_dome_reg) {
          io.b_reg.get(r0).lock := true.B
        }
      } else {
        when (io.i_acc(r1).valid & io.i_acc(r1).lock & w_dome_bus) {
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
      val w_dome_bus = Wire(Bool())

      if (p.useDome) {
        w_dome_bus := (io.b_bus(r0).reg.dome.get === io.b_bus(r1).reg.dome.get)
      } else {
        w_dome_bus := true.B
      }

      if (nRegPrev(r0) != nRegPrev(r1)) {
        when (io.b_bus(r1).reg.valid & io.b_bus(r1).reg.lock & w_dome_bus) {
          io.b_bus(r0).lock := true.B
        }
      }

      if (p.useReqReg) {
        val w_dome_reg = Wire(Bool())

        if (p.useDome) {
          w_dome_reg := (io.b_reg.get(r0).reg.dome.get === io.b_reg.get(r1).reg.dome.get)
        } else {
          w_dome_reg := true.B
        }

        when (io.b_reg.get(r1).reg.valid & io.b_reg.get(r1).reg.lock & w_dome_reg) {
          io.b_reg.get(r0).lock := true.B
        }
      }
    }
  }
}

object ReqStage extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new ReqStage(PrevUnitConfigBase), args)
}