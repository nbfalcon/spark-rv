package org.nbfalcon.sparkRV.main

import circt.stage.ChiselStage
import org.nbfalcon.sparkRV.SimpleCPU

object Main {
  def main(args: Array[String]): Unit = {
    val out = ChiselStage.emitSystemVerilog(new SimpleCPU())
    print(out)
  }
}
