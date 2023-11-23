package org.nbfalcon.sparkRV

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import firrtl.annotations.MemoryLoadFileType
import org.nbfalcon.sparkRV.Base.Word
import org.nbfalcon.sparkRV.util.SignExtend.SignExtend

class UnalignedRead extends Module {
  val io = IO(new Bundle {
    val wordA = Input(Word)
    val wordANext = Input(Word)

    val byteAddr = Input(UInt(2.W))
    val memWidth = Input(MemIOWidth())

    val result = Output(Word)
  })

  val lByte = Wire(UInt(8.W))
  lByte := DontCare
  switch(io.byteAddr) {
    is(0.U) {
      // little endian!
      lByte := io.wordA(7, 0)
    }
    is(1.U) {
      lByte := io.wordA(15, 8)
    }
    is(2.U) {
      lByte := io.wordA(23, 16)
    }
    is(3.U) {
      lByte := io.wordA(31, 24)
    }
  }

  val lHalf = Wire(UInt(16.W))
  lHalf := DontCare
  switch(io.byteAddr) {
    is(0.U) {
      lHalf := io.wordA(15, 0)
    }
    is(1.U) {
      lHalf := io.wordA(23, 8)
    }
    is(2.U) {
      lHalf := io.wordA(31, 16)
    }
    is(3.U) {
      lHalf := io.wordANext(7, 0) ## io.wordA(31, 24)
    }
  }

  val lWord = Wire(UInt(32.W))
  lWord := DontCare
  switch(io.byteAddr) {
    is(0.U) {
      lWord := io.wordA
    }
    is(1.U) {
      lWord := io.wordANext(7, 0) ## io.wordA(31, 8)
    }
    is(2.U) {
      lWord := io.wordANext(15, 0) ## io.wordA(31, 16)
    }
    is(3.U) {
      lWord := io.wordANext(23, 0) ## io.wordA(31, 24)
    }
  }

  import MemIOWidth._

  io.result := DontCare
  switch(io.memWidth) {
    is(M_BYTE) {
      io.result := lByte.signExtendT(Word)
    }
    is(M_HALF) {
      io.result := lHalf.signExtendT(Word)
    }
    is(M_WORD) {
      io.result := lWord
    }
    is(M_BYTE_UNSIGNED) {
      io.result := lByte
    }
    is(M_HALF_UNSIGNED) {
      io.result := lHalf
    }
  }
}

class UnalignedWrite extends Module {
  val io = IO(new Bundle {
    val wordA = Input(Word)
    val wordANext = Input(Word)

    val byteAddr = Input(UInt(2.W))
    val memWidth = Input(MemIOWidth())
    val writeWord = Input(Word)

    val resultA = Output(Word)
    val resultANext = Output(Word)

    val writeB = Output(Bool())
  })

  val byteShifted = MuxLookup(io.byteAddr, 0.U)(
    Seq.tabulate(4)(i => i.U -> (0.U(32.W) | ((io.writeWord & 0xFF.U) << i * 8).asUInt)))
  val byteMask = MuxLookup(io.byteAddr, 0.U)(
    Seq.tabulate(4)(i => i.U -> (0.U(32.W) | (0xFF.U << i * 8).asUInt)))
  val halfShifted = MuxLookup(io.byteAddr, 0.U)(
    Seq.tabulate(4)(i => i.U -> ((io.writeWord & 0xFFFF.U) << i * 8).asUInt))
  val halfMask = MuxLookup(io.byteAddr, 0.U)(
    Seq.tabulate(4)(i => i.U -> (0xFFFF.U << i * 8).asUInt))
  val wordShifted = MuxLookup(io.byteAddr, 0.U)(
    Seq.tabulate(4)(i => i.U -> (0.U((7 * 8).W) | ((io.writeWord & BigInt("FFFFFFFF", 16).U) << i * 8).asUInt)))
  val wordMask = MuxLookup(io.byteAddr, 0.U)(
    Seq.tabulate(4)(i => i.U -> (0.U((7 * 8).W) | (BigInt("FFFFFFFF", 16).U << i * 8).asUInt)))

  val halfRewrite = ((io.wordANext ## io.wordA) & (~halfMask).asUInt) | halfShifted
  val wordRewrite = ((io.wordANext ## io.wordA) & (~wordMask).asUInt) | wordShifted

  io.writeB := true.B
  io.resultA := DontCare
  io.resultANext := DontCare

  import MemIOWidth._

  switch(io.memWidth) {
    is(M_BYTE) {
      io.resultA := (byteShifted & byteMask) | (io.wordA & (~byteMask).asUInt)
      io.resultANext := io.wordA
      io.writeB := false.B
    }
    is(M_HALF) {
      io.resultA := halfRewrite(31, 0)
      io.resultANext := halfRewrite(63, 32)
      io.writeB := io.byteAddr === 3.U
    }
    is(M_WORD) {
      io.resultA := wordRewrite(31, 0)
      io.resultANext := wordRewrite(63, 32)
      io.writeB := io.byteAddr =/= 0.U
    }
  }
}

class Unrealistic3PortRWXMem(val initHex: String) extends Module {
  val memIO = IO(Flipped(new SimpleCPUMemInterface))

  // "Nobody needs more than 640k of memory"
  // Note that is has read-latency=0, write-latency=1!
  // (Not to be confused with SyncReadMem, which has read-latency=1)
  val memory = Mem(640000, Base.Word)
  loadMemoryFromFileInline(memory, initHex, MemoryLoadFileType.Hex)

  memIO.codeMemWord := memory.read((memIO.codeMemAddr >> 2).asUInt)

  val dataWordAddress = (memIO.dataMemAddr >> 2).asUInt
  val dataByteAddress = memIO.dataMemAddr(1, 0)

  val unalignedRead = Module(new UnalignedRead)
  unalignedRead.io.memWidth := memIO.dataMemIOMode
  unalignedRead.io.wordA := memory.read(dataWordAddress)
  unalignedRead.io.wordANext := memory.read(dataWordAddress + 1.U)
  unalignedRead.io.byteAddr := memIO.dataMemAddr & 0x3.U
  memIO.dataMemWord := unalignedRead.io.result

  val unalignedWrite = Module(new UnalignedWrite)
  unalignedWrite.io.memWidth := memIO.dataMemIOMode
  unalignedWrite.io.wordA := memory.read(dataWordAddress)
  unalignedWrite.io.wordANext := memory.read(dataWordAddress + 1.U)
  unalignedWrite.io.byteAddr := memIO.dataMemAddr & 0x3.U
  unalignedWrite.io.writeWord := memIO.dataMemWordWrite
  when (memIO.dataMemStore) {
    memory.write(dataWordAddress, unalignedWrite.io.resultA)
    memory.write(dataWordAddress + 1.U, unalignedWrite.io.resultANext)
  }
}
