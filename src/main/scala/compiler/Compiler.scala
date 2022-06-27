package compiler

import value._
import vm._
import vm.opcode._

import scala.collection.mutable

object Compiler {
  // Constants
  val TRUE = "true"
  val FALSE = "false"

  // Special forms
  val DO = "do"
  val IF = "if"
  val DEF = "def"
  val LAMBDA = "lambda*"
  val QUOTE = "quote"

  // Compilation level
  val DEF_MACRO = "defmacro"
}

class Compiler(vm: Vm = new Vm) {
  private val symbolTable = new SymbolTable()
  private val macros = new mutable.HashMap[java.lang.String, Function[OpCode]]()

  def compile(values: scala.List[Value[OpCode]]): Array[OpCode] = {
    val block = List[OpCode](
      Symbol(Compiler.DO) :: values
    )

    val compiler = new CompilerLoop
    compiler.compile(block)
    compiler.collect()
  }

  private class CompilerLoop(val symbolTable: SymbolTable = symbolTable) {
    private val emitter = new Emitter()

    def collect(): Array[OpCode] = emitter.collect

    def compile(value: Value[OpCode]): Unit = value match {
      case Number(_) | String(_) | Symbol(Compiler.TRUE) | Symbol(Compiler.FALSE) | Function(_, _) | Closure(_, _) =>
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
        case Nil => emitter.emit(Push(value))

        case Symbol(Compiler.DO) :: block => compileBlock(block)

        case Symbol(Compiler.IF) :: args => args match {
          case scala.List(cond) => compileIf(cond)
          case scala.List(cond, a) => compileIf(cond, a)
          case scala.List(cond, a, b) => compileIf(cond, a, b)
          case _ => throw new Exception("Invalid `if` arity")
        }

        case Symbol(Compiler.DEF) :: args => args match {
          case Symbol(name) :: args2 => args2 match {
            case Nil => compileDef(name)
            case value :: Nil => compileDef(name, value)
            case _ => throw new Exception("Invalid `def` arity")
          }

          case _ => throw new Exception("Invalid `def` arguments")
        }

        case Symbol(Compiler.DEF_MACRO) :: args => args match {
          case Symbol(name) :: List(params) :: body :: Nil => compileMacro(name, params, body)
          case _ => throw new Exception("Invalid `defmacro` arguments")
        }

        case Symbol(Compiler.LAMBDA) :: args => args match {
          case List(params) :: body :: Nil => compileLambda(params, body)
          case _ => throw new Exception("Invalid `lambda` arguments")
        }

        case Symbol(Compiler.QUOTE) :: args => args match {
          case value :: Nil => emitter.emit(Push(value))
          case _ => throw new Exception("Invalid `quote` arguments")
        }

        case Symbol("builtin/apply") :: f :: lst :: Nil =>
          compile(f)
          compile(lst)
          emitter.emit(Apply)

        case Symbol("builtin/fork") :: f :: Nil =>
          compile(f)
          emitter.emit(Fork)

        case Symbol("builtin/send") :: pid :: value :: Nil =>
          compile(pid)
          compile(value)
          emitter.emit(Send)

        case Symbol("builtin/receive") :: Nil =>
          emitter.emit(Receive)

        case Symbol("builtin/self") :: args => compileOp0(Self, args)


        case Symbol("builtin/add") :: args => compileOp2(Add, args)
        case Symbol("builtin/log") :: args => compileOp1(Log, args)
        case Symbol("builtin/greater-than") :: args => compileOp2(GreaterThan, args)
        case Symbol("builtin/not") :: args => compileOp1(Not, args)
        case Symbol("builtin/cons") :: args => compileOp2(Cons, args)
        case Symbol("builtin/first") :: args => compileOp1(First, args)
        case Symbol("builtin/rest") :: args => compileOp1(Rest, args)
        case Symbol("builtin/is-nil") :: args => compileOp1(IsNil, args)
        case Symbol("builtin/sleep") :: args => compileOp1(Sleep, args)
        case Symbol("builtin/panic") :: args => compileOp1(Panic, args)
        case Symbol("builtin/is-eq") :: args => compileOp2(IsEq, args)
        case Symbol("builtin/is-list") :: args => compileOp1(IsList, args)

        case f :: args => compileApplication(f, args)
      }
    }


    private def compileApplication(f: Value[OpCode], args: scala.List[Value[OpCode]]): Unit =
      lookupMacro(f) match {
        case Some(macroFunction) =>
          val instructions = scala.List[scala.List[OpCode]](
            args.map(Push),
            scala.List(
              Push(macroFunction),
              Call(args.length),
            )
          ).flatten.toArray

          val result = vm.run(instructions)
          compile(result)

        case None =>
          for (arg <- args) {
            compile(arg)
          }
          compile(f)
          emitter.emit(Call(args.length))
      }

    private def compileLambda(params: scala.List[Value[OpCode]], body: Value[OpCode] = List.of()): Unit = {
      val lambdaSymbolTable = symbolTable.nested
      val fn = CompilerLoop.compileLambda(lambdaSymbolTable, params, body)

      if (lambdaSymbolTable.freeSymbols.isEmpty) {
        emitter.emit(Push(fn))
      } else {
        for (free <- lambdaSymbolTable.freeSymbols) {
          emitter.emit(free.scope match {
            case Global => GetGlobal(free.index)
            case Local => GetLocal(free.index)
            case Free => GetFree(free.index)
          })
        }
        emitter.emit(
          PushClosure(
            freeVariables = lambdaSymbolTable.freeSymbols.length,
            fn = fn
          )
        )
      }
    }

    private def compileMacro(name: java.lang.String, params: scala.List[Value[OpCode]], body: Value[OpCode] = List.of()): Unit = {
      val lambdaSymbolTable = symbolTable.nested
      val fn = CompilerLoop.compileLambda(lambdaSymbolTable, params, body)
      macros.put(name, fn)
      emitter.emit(Push(Nil))
    }

    private def lookupMacro(value: Value[OpCode]): Option[Function[OpCode]] = value match {
      case Symbol(name) => macros get name
      case _ => None
    }

    private def compileDef(name: java.lang.String, value: Value[OpCode] = List.of()): Unit = {
      val symbol = symbolTable.define(name, forceGlobal = true)
      compile(value)
      emitter.emit(SetGlobal(symbol.index))
    }

    private def compileBlock(block: scala.List[Value[OpCode]]): Unit = block match {
      case Nil => emitter.emit(Push(Nil))
      case value :: Nil => compile(value)
      case _ => for ((value, index) <- block.zipWithIndex) {
        if (index != 0) {
          emitter.emit(Pop)
        }

        compile(value)
      }
    }

    private def compileIf(
                           cond: Value[OpCode],
                           branchTrue: Value[OpCode] = Nil,
                           branchFalse: Value[OpCode] = Nil
                         ): Unit = {
      compile(cond)
      val beginBranchTrue = emitter.placeholder()
      compile(branchTrue)

      val beginBranchFalse = emitter.placeholder()
      beginBranchTrue.fill(JumpIfNot)
      compile(branchFalse)
      beginBranchFalse.fill(Jump)
    }

    private def compileOp0(op: OpCode, args: scala.List[Value[OpCode]]): Unit = args match {
      case Nil =>
        emitter.emit(op)

      case _ => throw new Exception(s"Invalid arity (expected 0, got ${args.length}")
    }

    private def compileOp1(op: OpCode, args: scala.List[Value[OpCode]]): Unit = args match {
      case scala.List(x) =>
        compile(x)
        emitter.emit(op)

      case _ => throw new Exception(s"Invalid arity (expected 1, got ${args.length}")
    }

    private def compileOp2(op: OpCode, args: scala.List[Value[OpCode]]): Unit = args match {
      case scala.List(x, y) =>
        compile(x)
        compile(y)
        emitter.emit(op)
      case _ => throw new Exception(s"Invalid arity (expected 2, got ${args.length})")
    }

  }

  private object CompilerLoop {
    private def compileLambda(
                               symbolTable: SymbolTable,
                               params: scala.List[Value[OpCode]],
                               body: Value[OpCode] = List.of()
                             ): Function[OpCode] = {
      val compiler = new CompilerLoop(symbolTable)

      val compiledParams = CompiledParams.compile(params.map {
        case Symbol(str) => str
        case value => throw new Exception(s"Invalid `lambda` parameter (expected a symbol, got `${value.show}` instead)")
      })
      for (param <- compiledParams.required) {
        compiler.symbolTable.define(param)
      }
      for (param <- compiledParams.optionals) {
        compiler.symbolTable.define(param)
      }
      for (param <- compiledParams.rest) {
        compiler.symbolTable.define(param)
      }

      compiler.compile(body)
      compiler.emitter.emit(Return)
      val instructions = compiler.collect()

      Function(
        instructions = instructions,
        arity = compiledParams.toArity,
      )
    }
  }
}


private case class CompiledParams(
                                   required: scala.List[java.lang.String] = Nil,
                                   optionals: scala.List[java.lang.String] = Nil,
                                   rest: Option[java.lang.String] = None,
                                 ) {

  def toArity: ArgumentsArity = ArgumentsArity(
    required = required.length,
    optionals = optionals.length,
    rest = rest.isDefined,
  )
}

private object CompiledParams {
  val OPTIONAL = "&opt"
  val REST = "&rest"

  def apply(args: java.lang.String*): CompiledParams = compile(args.toList)

  def compile(args: scala.List[java.lang.String]): CompiledParams =
    args match {
      case Nil => new CompiledParams()
      case OPTIONAL :: opts => compileOpt(opts)
      case REST :: args1 => compileRest(args1)
      case arg :: tl =>
        val rc = compile(tl)
        rc.copy(required = arg :: rc.required)
    }

  private def compileOpt(args: scala.List[java.lang.String]): CompiledParams =
    args match {
      case Nil => new CompiledParams()
      case OPTIONAL :: _ => throw new Exception("duplicate &opt is not allowed")
      case REST :: args1 => compileRest(args1)
      case arg :: tl =>
        val rc = compileOpt(tl)
        rc.copy(optionals = arg :: rc.optionals)
    }

  private def compileRest(args: scala.List[java.lang.String]): CompiledParams =
    args match {
      case Nil => throw new Exception("no labels after &rest")
      case OPTIONAL :: _ => throw new Exception("duplicate &opt is not allowed")
      case REST :: _ => throw new Exception("duplicate &rest is not allowed")
      case arg :: Nil => new CompiledParams(rest = Some(arg))
      case _ :: _ :: _ => throw new Exception("only one argument after &rest is allowed")
    }
}
