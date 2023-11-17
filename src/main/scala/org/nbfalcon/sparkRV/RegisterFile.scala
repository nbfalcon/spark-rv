package org.nbfalcon.sparkRV

import chisel3._
import org.nbfalcon.sparkRV.Base.{RegId, Word}

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
    when(io.rd =/= 0.U) {
      regFile(io.rd) := io.valueD
    }
  }
}
