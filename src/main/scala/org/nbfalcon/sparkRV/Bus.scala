package org.nbfalcon.sparkRV

import chisel3._
import chisel3.util._
import org.nbfalcon.sparkRV.Base.Word

import scala.collection.mutable

object MemoryFault extends ChiselEnum {
  val ADDR_INVALID, ALIGNMENT = Value
}

class MemoryAddress extends Bundle {
  val address = Input(Word)
  val ioWidth = Input(MemIOWidth())
}

class MemoryReadSide extends Bundle {
  val readEnable = Input(Bool())

  val result = Output(Word)
  val resultValid = Output(Bool())
}

class MemoryWriteSide extends Bundle {
  val writeEnable = Input(Bool())

  val storeWhat = Input(Word)
  val storeReady = Input(Word)
  val storeValid = Output(Bool())
}

class MemorySlavePort extends Bundle {
  val addr = new MemoryAddress
  val read = new MemoryReadSide
  val write = new MemoryWriteSide
}

class MemoryMasterPort extends Bundle {
  val slave = new MemorySlavePort
  val fault = Output(MemoryFault())
}

case class MemoryMapping(baseAddress: Int, regionWidth: Int) {
  def contains(address: UInt): Bool = {
    address >= baseAddress.U && address < (baseAddress + regionWidth).U
  }
}

class MemoryXBarInterconnectBuilder {
  val masters = mutable.ArrayBuffer.empty[MemoryMasterPort]
  val slaves = mutable.ArrayBuffer.empty[(MemoryMapping, MemorySlavePort)]

  // We could have more flexible 1:n slave ports, but don't do this for now
  def addSlavePort(mmap: MemoryMapping, slave: MemorySlavePort): Unit = {
    slaves.addOne((mmap, slave))
  }

  // No round-robin scheduling! The first port wins
  def newMasterPort(): MemoryMasterPort = {
    val master = new MemoryMasterPort
    masters.addOne(master)
    master
  }

  def build(): Unit = {
    for ((mmap, slave) <- slaves) {
      val whichMasterWins = Mux1H(masters.map(port =>
        mmap.contains(port.slave.addr.address)
          && (port.slave.read.readEnable || port.slave.write.writeEnable)).toSeq,
        masters.toSeq)
      slave :<= whichMasterWins.slave
    }

    // Are the addresses valid?
    for (master <- masters) {
      val isAddressValid = slaves.map { case (mmap, _) => mmap.contains(master.slave.addr.address) }.foldLeft(false.B)((_: Bool) || (_: Bool))
      when (!isAddressValid) {
        master.fault := MemoryFault.ADDR_INVALID
      }
    }
  }
}