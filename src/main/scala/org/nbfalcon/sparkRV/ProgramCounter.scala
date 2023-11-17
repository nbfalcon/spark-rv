package org.nbfalcon.sparkRV

import chisel3._
import chisel3.util._
import org.nbfalcon.sparkRV.Base.Word

class ProgramCounter extends Module {
  val io = IO(new Bundle {
    val jumpMode = Input(JumpMode())

    val jImm20 = Input(SInt(20.W))
    val shouldJump = Input(Bool())
    val indirectJumpValue = Input(Word)

    val currentPC = Output(Word)
  })

  val pc = RegInit(Word, 0.U)

  import JumpMode._

  when(io.shouldJump) {
    when(io.jumpMode === J_BRANCH || io.jumpMode === J_JAL) {
      pc := (pc.asSInt + io.jImm20).asUInt
    }.elsewhen(io.jumpMode === J_JALR) {
      pc := io.indirectJumpValue
    }.otherwise {
      // FIXME: assertion??
      printf("io.jumpMode has an illegal value!")
    }
  }.otherwise {
    pc := pc + 4.U
  }

  io.currentPC := pc
}
