package org.nbfalcon.sparkRV.test

import chisel3._
import chisel3.util.experimental.loadMemoryFromFileInline
import chiseltest._
import chiseltest.experimental.expose
import firrtl.annotations.MemoryLoadFileType
import org.nbfalcon.sparkRV.SimpleCPU
import org.scalatest.freespec.AnyFreeSpec

import java.nio.file.Files

class SimpleCPUTest(codeFile: String) extends Module {
  val dut = Module(new SimpleCPU())

  val codeMem = Mem(16384, UInt(32.W))
  loadMemoryFromFileInline(codeMem, codeFile, MemoryLoadFileType.Hex)

  dut.io.codeMemWord := codeMem.read((dut.io.codeMemAddr >> 2).asUInt)
  dut.io.dataMemWord := 0.U

  val regfile = expose(dut.registerFile.regFile)
}


object Util {
  def loadAssemblyExample(source: String): String = {
    val asmFile = Files.createTempFile("riscv-snippet-", ".S")
    val objFile = Files.createTempFile("riscv-snippet", ".o")
    val flatObj = Files.createTempFile("riscv-snippet", ".bin")
    val hexFile = Files.createTempFile("riscv-snippet", ".hex")

    Files.writeString(asmFile, source)
    val runAsm = Runtime.getRuntime.exec(Array[String](
      "riscv64-linux-gnu-as",
      "-march",
      "rv32i",
      asmFile.toAbsolutePath.toString,
      "-o",
      objFile.toAbsolutePath.toString))
    if (runAsm.waitFor() != 0) {
      val stderr = new String(runAsm.getErrorStream.readAllBytes())
      throw new RuntimeException(s"Assembler failed:\n$stderr")
    }

    val runObjcopy = Runtime.getRuntime.exec(Array[String](
      "riscv64-linux-gnu-objcopy", objFile.toAbsolutePath.toString, "-O", "binary", flatObj.toAbsolutePath.toString))
    if (runObjcopy.waitFor() != 0) {
      val stderr = new String(runAsm.getErrorStream.readAllBytes())
      throw new RuntimeException(s"Objcopy failed:\n$stderr")
    }

    val obj = Files.readAllBytes(flatObj)
    val verilogHex = new StringBuilder()
    val LETTERS = "0123456789ABCDEF"
    for (wordPtr <- 0 until obj.length by 4) {
      for (byteI <- 3 until -1 by -1) {
        val byte = obj(wordPtr + byteI)
        val lo = byte & 0xF
        val hi = (byte >> 4) & 0xF
        verilogHex.append(LETTERS(hi)).append(LETTERS(lo))
      }
      verilogHex.append("\r\n")
    }
    // Append 00.., since treadle pads the memory with last value (so that we get 0-padded memory)
    verilogHex.append("00000000\r\n")

    Files.delete(asmFile)
    Files.delete(objFile)
    Files.delete(flatObj)

    Files.writeString(hexFile, verilogHex)
    hexFile.toString
  }

  def testCPU(asm: String) = new SimpleCPUTest(loadAssemblyExample(asm))
}

class BasicExamples extends AnyFreeSpec with ChiselScalatestTester {
  "Should work for simple arithmetic" in {
    test(Util.testCPU(
      """
addi x1, zero, 100
addi x2, x1, 20
add x3, x1, x2
add x1, x1, x1
""")).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      dut.clock.step(100)
      assert(dut.regfile(1).peekInt() == 200)
    }
  }

  "We have loops :)" in {
    test(Util.testCPU(
      """
addi x1, zero, 0
addi x2, zero, 100
addi x3, zero, 0
loop:
addi x3, x3, 4
addi x1, x1, 1
blt x1, x2, loop
""")).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      // FIXME: refactor all this
      dut.clock.step(1024)
      assert(dut.regfile(3).peekInt() == 400)
    }
  }

  "LUIPC" in {
    test(Util.testCPU(
      """
li x1, 0xFFFFFFFF
addi x2, x1, 10
""")) { dut =>
      dut.clock.setTimeout(0)
      dut.clock.step(1024)
      assert(dut.regfile(1).peekInt() == BigInt("FFFFFFFF", 16) && dut.regfile(2).peekInt() == 9)
    }
  }
}
