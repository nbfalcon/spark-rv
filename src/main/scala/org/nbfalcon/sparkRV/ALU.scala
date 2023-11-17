package org.nbfalcon.sparkRV

import chisel3.util.{is, switch}
import chisel3.{Bool, Bundle, DontCare, Input, Module, Mux, Output}
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
