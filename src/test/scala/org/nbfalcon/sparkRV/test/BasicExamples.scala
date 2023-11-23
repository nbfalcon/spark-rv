package org.nbfalcon.sparkRV.test

import chisel3._
import chiseltest._
import chiseltest.experimental.expose
import org.nbfalcon.sparkRV.{SimpleCPU, Unrealistic3PortRWXMem}
import org.scalatest.freespec.AnyFreeSpec

import java.nio.file.Files

class SimpleCPUTest(codeFile: String) extends Module {
  val dut = Module(new SimpleCPU())

  val mem = Module(new Unrealistic3PortRWXMem(codeFile))
  mem.memIO <> dut.mem

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
    // FIXME: file leak
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

  "Load store" in {
    test(Util.testCPU(
      """
li t0, 10000
sw t0, (t0)
lw x1, (t0)

li t1, 10010
lw
""")) { dut =>
      dut.clock.setTimeout(0)
      dut.clock.step(1024)
      assert(dut.regfile(1).peekInt() == 100)
    }
  }

  "Word IO" in {
    test(Util.testCPU(
      """
# a0: target
# a1: nwords
# t0: i
li a0, 10000
li a1, 100

li t0, 0
loop:
slli t1, t0, 2
add t2, a0, t1
sw t0, (t2)
addi t0, t0, 1
blt t0, a1, loop

# s0: result
li s0, 0

li t0, 0
loopRead:
slli t1, t0, 2
add t2, a0, t1
lw t3, (t2)
add s0, s0, t3
addi t0, t0, 1
blt t0, a1, loopRead

mv x1, s0
""")).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      dut.clock.step(10024)
      assert(dut.regfile(1).peekInt() == 4950)
    }
  }

  "Halfword IO" in {
    test(Util.testCPU(
      """
# a0: target
# a1: nwords
# t0: i
li a0, 10000
li a1, 100

li t0, 0
loop:
slli t1, t0, 1
add t2, a0, t1
sh t0, (t2)
addi t0, t0, 1
blt t0, a1, loop

# s0: result
li s0, 0

li t0, 0
loopRead:
slli t1, t0, 1
add t2, a0, t1
lhu t3, (t2)
add s0, s0, t3
addi t0, t0, 1
blt t0, a1, loopRead

mv x1, s0
""")).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      dut.clock.step(10024)
      assert(dut.regfile(1).peekInt() == 4950)
    }
  }
}
