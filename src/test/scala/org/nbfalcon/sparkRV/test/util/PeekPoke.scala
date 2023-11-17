package org.nbfalcon.sparkRV.test.util

import chisel3.UInt

object PeekPoke {
  implicit class PeekToSigned(val lit: UInt) {
    def valS = {
      val v = lit.litValue.toLong
      val hi = v & (1 << (lit.getWidth - 1))
      val mask = ~(1L << lit.getWidth)
      if (hi != 0) {
        BigInt((~hi + 1) & mask)
      } else {
        BigInt(v)
      }
    }
  }
}
