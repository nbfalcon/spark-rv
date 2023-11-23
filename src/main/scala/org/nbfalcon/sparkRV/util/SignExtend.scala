package org.nbfalcon.sparkRV.util

import chisel3._

object SignExtend {
  implicit class SignExtend(val base: UInt) {
    def signExtend(toWidth: Int) = {
      val wire = Wire(SInt(toWidth.W))
      wire := base.asSInt
      wire.asUInt
    }

    def signExtendT(toType: UInt) = {
      signExtend(toType.getWidth)
    }
  }
}
