package compiler

import value._
import vm._

object Compiler {
  val DO = "do"
  val TRUE = "true"
  val FALSE = "false"
  val IF = "if"
  val DEF = "def"
  val LAMBDA = "lambda"
  val QUOTE = "quote"

  def compile(values: scala.List[Value[Nothing]]): Array[OpCode] = {
    val block = List[Nothing](
      Symbol(DO) :: values
    )

    val compiler = new Compiler()

    compiler.compile(block)

    compiler.collect()
  }
}

class Compiler {
  private val emitter = new Emitter()
  private var symbolTable = new SymbolTable()

  def collect(): Array[OpCode] = emitter.collect

  def compile(value: Value[Nothing]): Unit = value match {
    case Number(_) | String(_) | Symbol(Compiler.TRUE) | Symbol(Compiler.FALSE) | CompiledFunction(_, _) | Closure(_, _) =>
      emitter.emit(Push(value))

    case Symbol(name) => symbolTable.resolve(name) match {
      case None => throw new Error(s"Binding not found: $name")
      case Some(sym) => sym.scope match {
        case Global => emitter.emit(GetGlobal(sym.index))
        case Local => emitter.emit(GetLocal(sym.index))
        case Free => emitter.emit(GetFree(sym.index))
      }
    }

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

      case Symbol(Compiler.LAMBDA) :: args => args match {
        case List(params) :: body :: Nil => compileLambda(
          params.map {
            case Symbol(name) => name
            case _ => throw new Exception("Invalid `lambda` parameter (expected a string)")
          },
          body,
        )
        case _ => throw new Exception("Invalid `lambda` arguments")
      }

      case Symbol(Compiler.QUOTE) :: args => args match {
        case value :: Nil => emitter.emit(Push(value))
        case _ => throw new Exception("Invalid `quote` arguments")
      }

      case f :: args =>
        args.foreach {
          compile
        }
        compile(f)
        emitter.emit(Call(args.length))
    }
  }

  private def compileLambda(params: scala.List[java.lang.String], body: Value[Nothing] = List.of()): Unit = {
    val compiler = new Compiler()
    compiler.symbolTable = this.symbolTable.nested
    for (param <- params) {
      compiler.symbolTable.define(param)
    }

    compiler.compile(body)
    compiler.emitter.emit(Return)
    val instructions = compiler.collect()

    val fn = CompiledFunction(
      instructions = instructions,
      argsNumber = params.length,
    )

    if (compiler.symbolTable.freeSymbols.isEmpty) {
      emitter.emit(Push(fn))
    } else {
      for (free <- compiler.symbolTable.freeSymbols) {
        val opCode = free.scope match {
          case Global => GetGlobal(free.index)
          case Local => GetLocal(free.index)
          case Free => GetFree(free.index)
        }
        emitter.emit(opCode)
      }
      emitter.emit(
        PushClosure(
          freeVariables = compiler.symbolTable.freeSymbols.length,
          fn = fn
        )
      )
    }
  }

  private def compileDef(name: java.lang.String, value: Value[Nothing] = List.of()): Unit = {
    val symbol = symbolTable.define(name, forceGlobal = true)
    compile(value)
    emitter.emit(SetGlobal(symbol.index))
  }

  private def compileBlock(block: scala.List[Value[Nothing]]): Unit = block match {
    case Nil => emitter.emit(Push(Value.nil))
    case value :: Nil => compile(value)
    case _ => for ((value, index) <- block.zipWithIndex) {
      if (index != 0) {
        emitter.emit(Pop)
      }

      compile(value)
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

