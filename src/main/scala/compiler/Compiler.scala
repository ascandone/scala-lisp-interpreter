package compiler

import value._
import vm._

import scala.collection.mutable.ArrayBuffer

private class Emitter {
  private val opcodes: ArrayBuffer[OpCode] = ArrayBuffer()

  def placeholder(): Placeholder = {
    val placeholder = new Placeholder(this)
    emit(null)
    placeholder
  }

  def emit(op: OpCode*): Unit =
    opcodes.addAll(op)

  def collect: Array[OpCode] = opcodes.toArray

  class Placeholder(emitter: Emitter) {
    private val index = emitter.opcodes.length

    def fill(opCode: (Int) => OpCode): Unit = {
      emitter.opcodes(index) = opCode(emitter.opcodes.length)
    }
  }
}


private class Compiler {
  private val emitter = new Emitter()

  def collect(): Array[OpCode] = emitter.collect

  def compile(value: Value[Nothing]): Unit = value match {
    case Number(_) | String(_) | Symbol("true") | Symbol("false") => emitter.emit(Push(value))

    case List(forms) => forms match {
      case scala.Nil => emitter.emit(Push(value))

      case Symbol("do") :: block => compileBlock(block)

      case (Symbol("+") :: args) => compileOp2(Add, args)
      case (Symbol(">") :: args) => compileOp2(GreaterThan, args)
      case (Symbol("not") :: args) => compileOp1(Not, args)

      case Symbol("if") :: args => args match {
        case scala.List(cond) => compileIf(cond)
        case scala.List(cond, a) => compileIf(cond, a)
        case scala.List(cond, a, b) => compileIf(cond, a, b)
        case _ => throw new Exception("Invalid `if` arity")
      }

      case _ => ???
    }
  }

  private def compileBlock(block: scala.List[Value[Nothing]]): Unit = block match {
    case Nil => emitter.emit(Push(Value.nil))
    case value :: Nil => compile(value)
    case _ => for ((value, index) <- block.zipWithIndex) {
      if (index != 0) {
        this.emitter.emit(Pop)
      }

      this.compile(value)
    }
  }

  private def compileIf(
                         cond: Value[Nothing],
                         branchTrue: Value[Nothing] = Value.nil,
                         branchFalse: Value[Nothing] = Value.nil
                       ): Unit = {
    compile(cond)
    val beginBranchTrue = emitter.placeholder()
    compile(branchTrue)

    val beginBranchFalse = emitter.placeholder()
    beginBranchTrue.fill(JumpIfNot)
    compile(branchFalse)
    beginBranchFalse.fill(Jump)
  }

  private def compileOp1(op: Op1Impl, args: scala.List[Value[Nothing]]): Unit = args match {
    case scala.List(x) => emitter.emit(
      Push(x),
      Op1(op)
    )

    case _ => throw new Exception(s"Invalid arity (expected 1, got $args)")
  }

  private def compileOp2(op: Op2Impl, args: scala.List[Value[Nothing]]): Unit = args match {
    case scala.List(x, y) => emitter.emit(
      Push(x),
      Push(y),
      Op2(op)
    )

    case _ => throw new Exception(s"Invalid arity (expected 2, got $args)")
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
