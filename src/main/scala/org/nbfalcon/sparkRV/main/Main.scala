package org.nbfalcon.sparkRV.main

import circt.stage.ChiselStage
import org.nbfalcon.sparkRV.SimpleCPU


object Main {
  def main(args: Array[String]): Unit = {
    val out = ChiselStage.emitFIRRTLDialect(new SimpleCPU)
    print(out)
  }
}
