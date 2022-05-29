package compiler

import value._
import vm._

import scala.collection.mutable

object Compiler {
  val DO = "do"
  val TRUE = "true"
  val FALSE = "false"
  val IF = "if"
  val DEF = "def"
  val LAMBDA = "lambda"

  def compile(values: scala.List[Value[Nothing]]): Array[OpCode] = {
    val block = List[Nothing](
      Symbol(DO) :: values
    )

    val compiler = new Compiler()

    compiler.compile(block)

    compiler.collect()
  }
}

private class Compiler {
  // TODO this should be outside
  private val globals = new mutable.HashMap[java.lang.String, Int]()

  private val emitter = new Emitter()

  def collect(): Array[OpCode] = emitter.collect

  def compile(value: Value[Nothing]): Unit = value match {
    case Number(_) | String(_) | Symbol(Compiler.TRUE) | Symbol(Compiler.FALSE) => emitter.emit(Push(value))

    case Symbol(name) =>
      val ident = globals(name)
      emitter.emit(GetGlobal(ident))

    case List(forms) => forms match {
      case scala.Nil => emitter.emit(Push(value))

      case Symbol(Compiler.DO) :: block => compileBlock(block)

      case Symbol("+") :: args => compileOp2(Add, args)
      case Symbol(">") :: args => compileOp2(GreaterThan, args)
      case Symbol("!") :: args => compileOp1(Not, args)

      case Symbol(Compiler.IF) :: args => args match {
        case scala.List(cond) => compileIf(cond)
        case scala.List(cond, a) => compileIf(cond, a)
        case scala.List(cond, a, b) => compileIf(cond, a, b)
        case _ => throw new Exception("Invalid `if` arity")
      }

      case Symbol(Compiler.DEF) :: args => args match {
        case Symbol(name) :: args2 => args2 match {
          case scala.Nil => compileDef(name)
          case value :: scala.Nil => compileDef(name, value)
          case _ => throw new Exception("Invalid `def` arity")
        }

        case _ => throw new Exception("Invalid `def` arguments")
      }
    }
  }

  private def compileDef(name: java.lang.String, value: Value[Nothing] = List.of()): Unit = {
    val ident = this.globals.size
    this.globals.put(name, ident)
    compile(value)
    emitter.emit(SetGlobal(ident))
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
    case scala.List(x) =>
      compile(x)
      emitter.emit(Op1(op))

    case _ => throw new Exception(s"Invalid arity (expected 1, got $args)")
  }

  private def compileOp2(op: Op2Impl, args: scala.List[Value[Nothing]]): Unit = args match {
    case scala.List(x, y) =>
      compile(x)
      compile(y)
      emitter.emit(Op2(op))

    case _ => throw new Exception(s"Invalid arity (expected 2, got $args)")
  }
}

