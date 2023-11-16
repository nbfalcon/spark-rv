package org.nbfalcon.sparkRV.test

import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3._
import chiseltest._
import chiseltest.experimental.expose
import firrtl.annotations.MemoryLoadFileType
import org.nbfalcon.sparkRV.SimpleCPU
import org.scalatest.freespec.AnyFreeSpec

import java.nio.file.Files

class SimpleCPUTest(codeFile: String) extends Module {
  val dut = Module(new SimpleCPU())

  val codeMem = SyncReadMem(16384, UInt(32.W))
  loadMemoryFromFileInline(codeMem, codeFile, MemoryLoadFileType.Hex)

  dut.io.codeMemWord := codeMem(dut.io.codeMemAddr)
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
      for (byteI <- 0 until 4) {
        val byte = obj(wordPtr + byteI)
        val lo = byte & 0xF
        val hi = (byte >> 4) & 0xF
        verilogHex.append(LETTERS(hi)).append(LETTERS(lo))
      }
      verilogHex.append("\r\n")
    }

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
.option norvc
mv t0, zero
addi t0, t0, 100
addi t0, t0, -50
add t1, t0, t0
add t0, t0, t0

mv x1, t0
mv x2, t1
""")) { dut =>
      dut.clock.setTimeout(0)
      dut.clock.step(1024)
      dumpCPU(dut)
    }
  }

  def dumpCPU(dut: SimpleCPUTest): Unit = {
    println("------ State snapshot")
//    println(s"pc at: ${cpu.pc.io.currentPC.peek()}")
    for ((reg, i) <- dut.regfile.zipWithIndex) {
      println(s"x$i: ${reg.peek()}")
    }
    println()
  }
}
