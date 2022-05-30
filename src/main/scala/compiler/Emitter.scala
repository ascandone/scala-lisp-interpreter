package compiler

import vm.OpCode

import scala.collection.mutable.ArrayBuffer

private class Emitter {
  private val opcodes: ArrayBuffer[OpCode] = ArrayBuffer()

  def placeholder(): Placeholder = {
    val placeholder = new Placeholder
    emit(null)
    placeholder
  }

  def emit(op: OpCode*): Unit =
    opcodes.addAll(op)

  def collect: Array[OpCode] = opcodes.toArray

  class Placeholder {
    private val index = opcodes.length

    def fill(opCode: (Int) => OpCode): Unit = {
      opcodes(index) = opCode(opcodes.length)
    }
  }
}
