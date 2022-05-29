package compiler

import value._
import vm._

import scala.collection.mutable.ArrayBuffer

private class Emitter {
  private val opcodes: ArrayBuffer[OpCode] = ArrayBuffer()

  def emit(op: OpCode*): Unit =
    opcodes.addAll(op)

  def collect: Array[OpCode] = opcodes.toArray
}


private class Compiler {
  private val emitter = new Emitter()

  def collect(): Array[OpCode] = emitter.collect

  def compile(value: Value[Nothing]): Unit = value match {
    case Number(_) => emitter.emit(Push(value))
    case String(_) => emitter.emit(Push(value))
    case Symbol("true") => emitter.emit(Push(value))
    case Symbol("false") => emitter.emit(Push(value))
    case List(forms) => forms match {
      case scala.Nil => emitter.emit(Push(value))

      case Symbol("do") :: Nil => emitter.emit(Push(Value.nil))
      case Symbol("do") :: value :: Nil => compile(value)
      case Symbol("do") :: block =>
        for ((value, index) <- block.zipWithIndex) {
          if (index != 0) {
            this.emitter.emit(Pop)
          }

          this.compile(value)
        }

      case (Symbol("+") :: args) => compileOp2(Add, args)
      case (Symbol(">") :: args) => compileOp2(GreaterThan, args)

      case _ => ???
    }
  }

  private def compileOp2(op: Op2Impl, args: scala.List[Value[Nothing]]): Unit = args match {
    case scala.List(x, y) => emitter.emit(
      Push(x),
      Push(y),
      Op2(op)
    )

    // TODO better err
    case _ => throw new Exception("Invalid arity")
  }
}


object Compiler {
  def compile(values: scala.List[Value[Nothing]]): Array[OpCode] = {
    val block = List[Nothing](
      Symbol("do") :: values
    )

    val compiler = new Compiler()

    compiler.compile(block)

    compiler.collect()
  }
}
