package org.nbfalcon.sparkRV

import chisel3._
import chisel3.util._
import org.nbfalcon.sparkRV.Base.Word

class ALU extends Module {
  val io = IO(new Bundle {
    val aluOP = Input(RFunct3())
    val aluNegate = Input(Bool())
    val value1 = Input(Word)
    val value2 = Input(Word)
    val result = Output(Word)
  })

  import RFunct3._
  // io_result is not fully initialized (actually it is, because our switch is exhaustive)
  io.result := DontCare
  switch(io.aluOP) {
    is(R_ADD_SUB) {
      io.result := Mux(!io.aluNegate, io.value1 + io.value2, io.value1 - io.value2)
    }
    is(R_SLL_SLA) {
      val shamt = io.value2(4, 0)
      val logicalShift = io.value1 << shamt
      val arithmeticShift = (io.value1.asSInt << shamt).asUInt
      io.result := Mux(io.aluNegate, arithmeticShift, logicalShift)
    }
    is(R_SLT) {
      io.result := io.value1.asSInt < io.value2.asSInt
    }
    is(R_SLTU) {
      io.result := io.value1 < io.value2
    }
    is(R_XOR) {
      io.result := io.value1 ^ io.value2
    }
    is(R_SRL_SRA) {
      val shamt = io.value2(4, 0)
      val logicalShift = io.value1 >> shamt
      val arithmeticShift = (io.value1.asSInt >> shamt).asUInt
      io.result := Mux(io.aluNegate, arithmeticShift, logicalShift)
    }
    is(R_OR) {
      io.result := io.value1 | io.value2
    }
    is(R_AND) {
      io.result := io.value1 & io.value2
    }
  }
}

class JumpALU extends Module {
  val io = IO(new Bundle {
    val value1 = Input(Word)
    val value2 = Input(Word)

    val jumpType = Input(CJumpType())

    val shouldJump = Output(Bool())
  })

  // FIXME: illegal??
  io.shouldJump := DontCare
  import CJumpType._
  // FIXME: handle illegal insns-
  switch(io.jumpType) {
    is(C_BEQ) {
      io.shouldJump := io.value1 === io.value2
    }
    is(C_BNE) {
      io.shouldJump := io.value1 === io.value2
    }
    is(C_BLT) {
      io.shouldJump := io.value1.asSInt < io.value2.asSInt
    }
    is(C_BGE) {
      io.shouldJump := io.value2.asSInt > io.value1.asSInt
    }
    is(C_BLTU) {
      io.shouldJump := io.value1 < io.value2
    }
    is(C_BGEU) {
      io.shouldJump := io.value2 > io.value1
    }
  }
}
