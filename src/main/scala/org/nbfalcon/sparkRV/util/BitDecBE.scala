package org.nbfalcon.sparkRV.util

import chisel3._

class BitDecBE(size: Int, val inputWord: UInt) {
  private var currentBit = size - 1

  def next(width: Int): UInt = {
    require(currentBit - width >= -1)
    val decoder = Wire(UInt(width.W))
    decoder := inputWord(currentBit, currentBit - width + 1)
    skip(width)
    decoder
  }

  def skip(width: Int): Unit = {
    require(currentBit - width >= -1)
    currentBit -= width
  }

  def done(): Unit = {
    require(currentBit == -1)
  }
}
object BitDecBE {
  def apply(inputWord: UInt) = new BitDecBE(inputWord.getWidth, inputWord)
}

// Prototype a new API
// val i = new BitDecBE() { val imm12 = next(...) } -> iImm12 becomes i.imm2