package org.nbfalcon.sparkRV

import chisel3._
import chisel3.util._
import org.nbfalcon.sparkRV.Base._

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
  decoder.instructionRegister := io.codeMemWord

  val registerFile = Module(new RegisterFile())
  registerFile.io.rs1 := decoder.ctl.rs1
  registerFile.io.rs2 := decoder.ctl.rs2
  registerFile.io.rd := decoder.ctl.rd

  val value1: UInt = registerFile.io.value1
  val value2: UInt = registerFile.io.value2

  val alu = Module(new ALU())
  alu.io.value1 := value1
  // FIXME: the second value should not be converted to SInt!! We need a "widen to this width" operator
  alu.io.value2 := Mux(decoder.ctl.aluUseImmediate, decoder.ctl.imm12.asSInt, value2.asSInt).asUInt
  alu.io.aluOP := decoder.ctl.aluOP
  alu.io.aluNegate := decoder.ctl.aluNegate

  val jumpALU = Module(new JumpALU())
  jumpALU.io.value1 := value1
  jumpALU.io.value2 := value2
  jumpALU.io.jumpType := decoder.ctl.cJumpType

  val shouldJump = Wire(Bool())
  val andLink = Wire(Bool())
  shouldJump := false.B
  andLink := false.B
  import JumpMode._
  switch(decoder.ctl.jumpMode) {
    is(J_JALR, J_JAL) {
      shouldJump := true.B
      andLink := true.B
    }
    is(J_BRANCH) {
      shouldJump := jumpALU.io.shouldJump
    }
  }
  pc.io.shouldJump := shouldJump
  pc.io.jumpMode := decoder.ctl.jumpMode
  pc.io.indirectJumpValue := value1
  pc.io.jImm20 := Mux(decoder.ctl.jumpMode === J_JAL, decoder.ctl.jalImm20, decoder.ctl.bImm12)

  import Opcode._
  when(andLink) {
    registerFile.io.valueD := pc.io.currentPC + 4.U
  }.elsewhen(decoder.ctl.opcode === OP_LUIPC) {
    registerFile.io.valueD := decoder.ctl.uImm20 << 12
  }.elsewhen(decoder.ctl.opcode === OP_AUIPC) {
    registerFile.io.valueD := pc.io.currentPC + (decoder.ctl.uImm20 << 12)
  }.otherwise {
    registerFile.io.valueD := alu.io.result
  }
  registerFile.io.storeRd := decoder.ctl.aluStoreRd || decoder.ctl.memLoad ||
    (decoder.ctl.jumpMode === J_JALR || decoder.ctl.jumpMode === J_JALR) ||
    (decoder.ctl.opcode === OP_LUIPC || decoder.ctl.opcode === OP_AUIPC)

  io.dataMemAddr := value1
  io.dataMemLoad := decoder.ctl.memLoad
  io.dataMemStore := decoder.ctl.memStore
  io.dataMemIOMode := decoder.ctl.memIOMode
}