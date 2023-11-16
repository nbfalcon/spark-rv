package org.nbfalcon.sparkRV

import chisel3._
import chisel3.util._

object Base {
  def RegId = UInt(5.W)
  def Imm12 = UInt(12.W)
  def Word = UInt(32.W)
}