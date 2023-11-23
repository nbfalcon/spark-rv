package org.nbfalcon.sparkRV.test

import chisel3._
import chiseltest._
import chiseltest.experimental.expose
import org.nbfalcon.sparkRV.test.util.Util
import org.nbfalcon.sparkRV.{SimpleCPU, Unrealistic3PortRWXMem}
import org.scalatest.freespec.AnyFreeSpec

import java.nio.file.Files

class SimpleCPUTest(codeFile: String) extends Module {
  val dut = Module(new SimpleCPU())

  val mem = Module(new Unrealistic3PortRWXMem(codeFile))
  mem.memIO <> dut.mem

  val regfile = expose(dut.registerFile.regFile)
}

class CPUAsmTestBase extends AnyFreeSpec with ChiselScalatestTester {
  def testExample(code: String)(testDut: SimpleCPUTest => Unit): TestResult = {
    val file = Util.loadAssemblyExample(code)
    test(new SimpleCPUTest(file.toString)) { dut =>
      Files.deleteIfExists(file)

      dut.clock.setTimeout(0)
      dut.clock.step(10000)

      testDut(dut)
    }
  }
}

class BasicExamples extends CPUAsmTestBase {
  "Should work for simple arithmetic" in {
    testExample("""
addi x1, zero, 100
addi x2, x1, 20
add x3, x1, x2
add x1, x1, x1
""") { dut =>
      assert(dut.regfile(1).peekInt() == 200)
    }
  }

  "We have loops :)" in {
    testExample("""
addi x1, zero, 0
addi x2, zero, 100
addi x3, zero, 0
loop:
addi x3, x3, 4
addi x1, x1, 1
blt x1, x2, loop
""") { dut =>
      assert(dut.regfile(3).peekInt() == 400)
    }
  }

  "LUIPC" in {
    testExample("""
li x1, 0xFFFFFFFF
addi x2, x1, 10
""") { dut =>
      assert(dut.regfile(1).peekInt() == BigInt("FFFFFFFF", 16) && dut.regfile(2).peekInt() == 9)
    }
  }

  "Load store" in {
    testExample("""
li t0, 10000
sw t0, (t0)
lw x1, (t0)
""") { dut =>
      assert(dut.regfile(1).peekInt() == 10000)
    }
  }

  "Word IO" in {
    testExample("""
# a0: target
# a1: n(words)
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
""") { dut =>
      assert(dut.regfile(1).peekInt() == 4950)
    }
  }

  "Halfword IO" in {
    testExample("""
# a0: target
# a1: n(words)
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
""") { dut =>
      assert(dut.regfile(1).peekInt() == 4950)
    }
  }
}
