package org.nbfalcon.sparkRV.test

import chisel3._
import chiseltest._
import org.nbfalcon.sparkRV.RegisterFile
import org.scalatest.freespec.AnyFreeSpec

class RegisterFileTest extends AnyFreeSpec with ChiselScalatestTester {
  "Basic" in {
    test(new RegisterFile()).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.valueD.poke(0.U)
      dut.io.rs1.poke(0.U)
      dut.io.rs2.poke(0.U)
      dut.io.rd.poke(0.U)

      dut.io.rd.poke(2.U)
      dut.io.valueD.poke(100.U)
      dut.io.storeRd.poke(true.B)
      dut.clock.step()

      dut.io.storeRd.poke(false.B)
      dut.io.rs1.poke(2.U)
      assert(dut.io.value1.peekInt() == 100)
    }
  }
}
