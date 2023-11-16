package org.nbfalcon.sparkRV

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import firrtl.annotations.MemoryLoadFileType
import org.nbfalcon.sparkRV.Base._

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

class RegisterFile extends Module {
  val io = IO(new Bundle {
    val rs1 = Input(RegId)
    val rs2 = Input(RegId)
    val rd = Input(RegId)
    val storeRd = Input(Bool())

    val value1 = Output(Word)
    val value2 = Output(Word)
    val valueD = Input(Word)
  })

  val regFile = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  io.value1 := Mux(io.rs1 === 0.U, 0.U, regFile(io.rs1))
  io.value2 := Mux(io.rs2 === 0.U, 0.U, regFile(io.rs2))

  when(io.storeRd) {
    // io.rd == 0 -> don't care
    regFile(io.rd) := io.valueD
  }
}

class ProgramCounter extends Module {
  val io = IO(new Bundle {
    val currentPC = Output(Word)
  })

  val pc = RegInit(Word, 0.U)
  pc := pc + 4.U

  io.currentPC := pc
}

class SimpleCPUIO extends Bundle {
  val codeMemAddr = Output(Word)
  val codeMemWord = Input(Word)

  val dataMemLoad = Output(Bool())
  val dataMemStore = Output(Bool())
  val dataMemAddr = Output(Word)
  val dataMemWord = Input(Word)
  val dataMemIOMode = Output(MemIOMode())
}

class SimpleCPU extends Module {
  val io = IO(new SimpleCPUIO)

  val pc = Module(new ProgramCounter())

  io.codeMemAddr := pc.io.currentPC

  val decoder = Module(new Decoder())
  decoder.io.instructionRegister := io.codeMemWord

  val registerFile = Module(new RegisterFile())
  registerFile.io.rs1 := decoder.io.rs1
  registerFile.io.rs2 := decoder.io.rs2
  registerFile.io.rd := decoder.io.rd
  registerFile.io.storeRd := decoder.io.aluStoreRd || decoder.io.memLoad

  val value1: UInt = registerFile.io.value1
  val value2: UInt = registerFile.io.value2

  val alu = Module(new ALU())
  alu.io.value1 := value1
  alu.io.value2 := Mux(decoder.io.aluUseImmediate, decoder.io.imm12, value2)
  alu.io.aluOP := decoder.io.aluOP
  alu.io.aluNegate := decoder.io.aluNegate
  registerFile.io.valueD := alu.io.result

  io.dataMemAddr := value1
  io.dataMemLoad := decoder.io.memLoad
  io.dataMemStore := decoder.io.memStore
  io.dataMemIOMode := decoder.io.memIOMode
}