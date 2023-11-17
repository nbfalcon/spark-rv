package org.nbfalcon.sparkRV

import chisel3._
import org.nbfalcon.sparkRV.Base.Word

class ProgramCounter extends Module {
  val io = IO(new Bundle {
    val currentPC = Output(Word)
  })

  val pc = RegInit(Word, 0.U)

  pc := pc + 4.U

  io.currentPC := pc
}
