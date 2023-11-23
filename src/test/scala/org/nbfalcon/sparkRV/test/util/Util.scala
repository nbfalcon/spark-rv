package org.nbfalcon.sparkRV.test.util

import java.nio.file.Files

object Util {
  def loadAssemblyExample(source: String) = {
    val asmFile = Files.createTempFile("riscv-snippet-", ".S")
    val objFile = Files.createTempFile("riscv-snippet", ".o")
    val flatObj = Files.createTempFile("riscv-snippet", ".bin")

    val hexFile = Files.createTempFile("riscv-snippet", ".hex")
    try {

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

      Files.writeString(hexFile, verilogHex)
      hexFile
    } finally {
      Files.deleteIfExists(asmFile)
      Files.deleteIfExists(objFile)
      Files.deleteIfExists(flatObj)
    }
  }
}
