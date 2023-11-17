package org.nbfalcon.sparkRV.test

import chisel3._
import chisel3.util.experimental.loadMemoryFromFileInline
import chiseltest._
import chiseltest.experimental.expose
import firrtl.annotations.MemoryLoadFileType
import org.nbfalcon.sparkRV.Base.Word
import org.nbfalcon.sparkRV.Decoder
import org.scalatest.freespec.AnyFreeSpec

class DecoderTestDut(codeFile: String) extends Module {
  val addr = IO(Input(Word))

  val dut = Module(new Decoder())

  val codeMem = SyncReadMem(16384, UInt(32.W))
  loadMemoryFromFileInline(codeMem, codeFile, MemoryLoadFileType.Hex)

  dut.instructionRegister := codeMem(addr)

  val dec = expose(dut.ctl)
}

class DecTest(val xs: (DecoderTestDut => Unit)*) {}

trait DecoderTestSpec extends AnyFreeSpec with ChiselScalatestTester {
  protected def example(assembly: String, testVec: DecTest): Unit = {
    test(new DecoderTestDut(Util.loadAssemblyExample(assembly))).withAnnotations(Seq(WriteVcdAnnotation)) {
      dut =>
        for ((tester, i) <- testVec.xs.zipWithIndex) {
          dut.addr.poke(i.U)
          dut.clock.step()
          if (tester != null) tester(dut)
        }
    }
  }
}

class DecoderTest extends DecoderTestSpec {
  "Should work for simple arithmetic" in {
    example(
      """
addi zero, zero, 2000
add x31, x31, x31
add x1, x1, x1
""", new DecTest(
        (dut => assert(dut.dec.imm12.peekInt() == 2000)),
        (dut => assert(dut.dec.rs1.peekInt() == 31 && dut.dec.rs2.peekInt() == 31 && dut.dec.rd.peekInt() == 31 && dut.dec.aluStoreRd.peekInt() == 1)),
        (dut => assert(dut.dec.rs1.peekInt() == 1 && dut.dec.rs2.peekInt() == 1 && dut.dec.rd.peekInt() == 1 && dut.dec.aluStoreRd.peekInt() == 1)),
      ))
  }
}
