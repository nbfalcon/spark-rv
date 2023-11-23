package org.nbfalcon.sparkRV

import chisel3._
import chisel3.util._
import chisel3.experimental.conversions.tuple2hwtuple
import org.nbfalcon.sparkRV.Base._
import org.nbfalcon.sparkRV.util.BitDecBE

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

object MemIOWidth extends ChiselEnum {
  val M_BYTE = Value(0x0.U)
  val M_HALF = Value(0x1.U)
  val M_WORD = Value(0x2.U)
  val M_BYTE_UNSIGNED = Value(0x4.U)
  val M_HALF_UNSIGNED = Value(0x5.U)
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

object CJumpType extends ChiselEnum {
  val C_BEQ = Value(0x0.U)
  val C_BNE = Value(0x1.U)
  val C_BLT = Value(0x4.U)
  val C_BGE = Value(0x5.U)
  val C_BLTU = Value(0x6.U)
  val C_BGEU = Value(0x7.U)
}

// The subset of jump opcodes
object JumpMode extends ChiselEnum {
  val J_BRANCH = Value(Opcode.OP_BRANCH.litValue.U)
  val J_JALR = Value(Opcode.OP_JALR.litValue.U)
  val J_JAL = Value(Opcode.OP_JAL.litValue.U)
}

class RVControl extends Bundle() {
  val opcode = Output(Opcode())
  val rs1 = Output(RegId)
  val rs2 = Output(RegId)
  val rd = Output(RegId)

  val aluOP = Output(RFunct3())
  val aluNegate = Output(Bool())
  val aluUseImmediate = Output(Bool())
  val imm12 = Output(UInt(12.W))
  val aluStoreRd = Output(Bool())

  val loadOffsetImm = Output(SInt(12.W))
  val storeOffsetImm = Output(SInt(12.W))
  val memIOMode = Output(MemIOWidth())
  val memUnsigned = Output(Bool())
  val memStore = Output(Bool())
  val memLoad = Output(Bool())

  val isAJumpOpcode = Output(Bool())
  val jumpMode = Output(JumpMode())
  val jalImm20 = Output(SInt(20.W))
  val cJumpType = Output(CJumpType())
  // Conditional Branch immediate; note that is 13-bits wide, due to extension/bitshifting
  val bImm12 = Output(SInt(13.W))

  val uImm20 = Output(UInt(20.W))
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

  val bType = BitDecBE(instructionRegister)
  val bImm12_12 = bType.next(1)
  val bImm12_10_5 = bType.next(6)
  bType.skip(RegId.getWidth)
  bType.skip(RegId.getWidth)
  bType.skip(3)
  val bImm12_4_1 = bType.next(4)
  val bImm12_11 = bType.next(1)
  bType.skip(7)
  bType.done()
  val bImm12 = Cat(bImm12_12, bImm12_11, bImm12_10_5, bImm12_4_1, 0.U(1.W))

  val uType = BitDecBE(instructionRegister)
  val uImm20 = uType.next(20)

  val jType = BitDecBE(instructionRegister)
  val jImm20 = jType.next(20)

  ctl.opcode := opcode

  ctl.rs2 := rs2
  ctl.rs1 := rs1
  ctl.rd := rd

  ctl.imm12 := imm12
  ctl.aluOP := funct3

  ctl.loadOffsetImm := imm12.asSInt
  ctl.storeOffsetImm := sImm12.asSInt
  ctl.memIOMode := MemIOWidth(funct3Raw)
  ctl.memUnsigned := funct3Raw(2)

  ctl.uImm20 := uImm20

  ctl.bImm12 := bImm12.asSInt
  ctl.jalImm20 := jImm20.asSInt
  ctl.cJumpType := CJumpType(funct3Raw)

  // defaults
  ctl.aluStoreRd := false.B
  ctl.aluUseImmediate := false.B
  ctl.isAJumpOpcode := false.B
  ctl.aluNegate := false.B

  import Opcode._
  switch(opcode) {
    is(OP_ARITH) {
      ctl.aluStoreRd := true.B
      // FIXME: we need to handle illegal funct7 opcodes
      ctl.aluNegate := funct7(1)
    }
    is(OP_IMM) {
      ctl.aluStoreRd := true.B
      ctl.aluUseImmediate := true.B
    }
    is(OP_LOAD) {
      ctl.aluStoreRd := true.B
    }
    is(OP_STORE) {
    }
  }
  ctl.memLoad := opcode === OP_LOAD
  ctl.memStore := opcode === OP_STORE
  (ctl.jumpMode, ctl.isAJumpOpcode) := JumpMode.safe(opcode.asUInt)
}
