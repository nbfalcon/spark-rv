package org.nbfalcon.sparkRV

import chisel3._
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
  registerFile.io.storeRd := decoder.ctl.aluStoreRd || decoder.ctl.memLoad

  val value1: UInt = registerFile.io.value1
  val value2: UInt = registerFile.io.value2

  val alu = Module(new ALU())
  alu.io.value1 := value1
  alu.io.value2 := Mux(decoder.ctl.aluUseImmediate, decoder.ctl.imm12, value2)
  alu.io.aluOP := decoder.ctl.aluOP
  alu.io.aluNegate := decoder.ctl.aluNegate
  registerFile.io.valueD := alu.io.result

  io.dataMemAddr := value1
  io.dataMemLoad := decoder.ctl.memLoad
  io.dataMemStore := decoder.ctl.memStore
  io.dataMemIOMode := decoder.ctl.memIOMode
}