package interpreter

import compiler.Compiler
import value.Value
import value.parser.Parser
import vm._

import scala.io.Source

object Interpreter {
  private lazy val preludeContent = {
    val res = Source.fromResource(PRELUDE_PATH)
    val value = Parser.run(res.mkString(""))
    value.get
  }

  private val PRELUDE_PATH = "prelude.lisp"

  def parseRun(src: java.lang.String, loadPrelude: Boolean = false): Value[OpCode] = {
    val interpreter = new Interpreter()
    if (loadPrelude) {
      interpreter.loadPrelude()
    }
    interpreter.parseEval(src)
  }
}

class Interpreter {
  private val vm = new Vm
  private val compiler = new Compiler(vm)

  def loadPrelude(): Interpreter = {
    eval(Interpreter.preludeContent)
    this
  }

  def parseEval(src: java.lang.String): Value[OpCode] = {
    val values = Parser.run(src).get
    eval(values)
  }

  def eval(values: scala.List[Value[Nothing]]): Value[OpCode] = values.map(value => {
    val compiled = compiler.compile(scala.List(value))
    vm.run(compiled)
  }).last
}
