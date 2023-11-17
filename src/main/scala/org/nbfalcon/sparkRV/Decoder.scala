package org.nbfalcon.sparkRV

import chisel3._
import chisel3.util._
import org.nbfalcon.sparkRV.Base._
import org.nbfalcon.sparkRV.util.BitDecBE

// TODO: ChiselEnum requires Value(s) to be monotonically increasing. I have sorted the lines below for that reason
//  fix this upstream??
//object Opcode extends ChiselEnum {
//  val OP_ARITH = Value("b0110011".U)
//  val OP_IMM = Value("b0010011".U)
//  val OP_LOAD = Value("b0000011".U)
//  val OP_STORE = Value("b0100011".U)
//  val OP_BRANCH = Value("b1100011".U)
//  val OP_JAL = Value("b1101111".U)
//  val OP_JALR = Value("b1100111".U)
//  val OP_LUIPC = Value("b0110111".U)
//  val OP_AUIPC = Value("b0010111".U)
//  val OP_ECALL = Value("b1110011".U)
//}

object Opcode extends ChiselEnum {
  val OP_LOAD = Value("b0000011".U)
  val OP_IMM = Value("b0010011".U)
  val OP_AUIPC = Value("b0010111".U)
  val OP_STORE = Value("b0100011".U)
  val OP_ARITH = Value("b0110011".U)
  val OP_LUIPC = Value("b0110111".U)
  val OP_BRANCH = Value("b1100011".U)
  val OP_JALR = Value("b1100111".U)
  val OP_JAL = Value("b1101111".U)
  val OP_ECALL = Value("b1110011".U)
}

object RFunct3 extends ChiselEnum {
  val R_ADD_SUB = Value("b000".U)
  val R_SLL_SLA = Value("b001".U)
  val R_SLT = Value("b010".U)
  val R_SLTU = Value("b011".U)
  val R_XOR = Value("b100".U)
  val R_SRL_SRA = Value("b101".U)
  val R_OR = Value("b110".U)
  val R_AND = Value("b111".U)
}

object MemIOMode extends ChiselEnum {
  val M_BYTE = Value(0.U)
  val M_HALF = Value(1.U)
  val M_WORD = Value(2.U)
}

class RVControl extends Bundle() {
  val rs1 = Output(RegId)
  val rs2 = Output(RegId)
  val rd = Output(RegId)

  val aluOP = Output(RFunct3())
  val aluNegate = Output(Bool())
  val aluUseImmediate = Output(Bool())
  val imm12 = Output(UInt(12.W))
  val aluStoreRd = Output(Bool())

  val loadOffsetImm = Output(UInt(12.W))
  val storeOffsetImm = Output(UInt(12.W))
  val memIOMode = Output(MemIOMode())
  val memUnsigned = Output(Bool())
  val memStore = Output(Bool())
  val memLoad = Output(Bool())
}


class Decoder extends Module {
  val ctl = IO(new RVControl)
  val instructionRegister = IO(Input(Word))

  val rType = BitDecBE(instructionRegister)
  val funct7 = rType.next(7)
  val rs2 = rType.next(RegId.getWidth)
  val rs1 = rType.next(RegId.getWidth)
  val funct3Raw: UInt = rType.next(3)
  val funct3 = RFunct3(funct3Raw)
  val rd = rType.next(RegId.getWidth)
  val opcode = suppressEnumCastWarning {
    Opcode(rType.next(7))
  }
  rType.done()

  val iType = BitDecBE(instructionRegister)
  val imm12 = iType.next(12)
  // FIXME: we can check that all encodings "match up" using a custom ad-hoc type and by registering all results
  // e.g. rs1 := iType.aliases(RegId.getWidth)

  val sType = BitDecBE(instructionRegister)
  val sImmHi7 = sType.next(7)
  sType.skip(RegId.getWidth)
  sType.skip(RegId.getWidth)
  sType.skip(3)
  val sImmLo5 = sType.next(5)
  val sImm12 = Cat(sImmHi7, sImmLo5)

  val uType = BitDecBE(instructionRegister)
  val uImm20 = uType.next(20)

  val aluUseImmediate = Wire(Bool())
  aluUseImmediate := false.B

  ctl.rs2 := rs2
  ctl.rs1 := rs1
  ctl.rd := rd

  ctl.imm12 := imm12
  ctl.aluOP := funct3

  ctl.loadOffsetImm := imm12
  ctl.storeOffsetImm := sImm12
  ctl.memIOMode := MemIOMode(funct3Raw(1, 0))
  ctl.memUnsigned := funct3Raw(2)
  ctl.memStore := false.B
  ctl.memLoad := false.B

  import Opcode._

  switch(opcode) {
    is(OP_ARITH) {
    }
    is(OP_IMM) {
      aluUseImmediate := true.B
    }
    is(OP_LOAD) {
      ctl.memLoad := true.B
      ctl.memStore := false.B
    }
    is(OP_STORE) {
      ctl.memLoad := false.B
      ctl.memStore := true.B
    }
  }

  ctl.aluStoreRd := opcode === OP_IMM || opcode === OP_ARITH
  ctl.aluUseImmediate := opcode === OP_IMM
  ctl.aluNegate := !aluUseImmediate && funct7(1) // Negates some instructions (all "dual" ambiguous in RFunct3)
}
