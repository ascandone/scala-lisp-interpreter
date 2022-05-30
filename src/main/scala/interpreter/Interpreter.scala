package interpreter

import compiler.Compiler
import value.Value
import value.parser.Parser
import vm._

object Interpreter {
  def parseRun(src: java.lang.String): Value[OpCode] = new Interpreter().parseEval(src)
}

class Interpreter {
  private val compiler = new Compiler()
  private val vm = new Vm

  def parseEval(src: java.lang.String): Value[OpCode] = {
    val parsed = Parser.run(src).get
    eval(parsed)
  }

  def eval(value: scala.List[Value[Nothing]]): Value[OpCode] = {
    val compiled = compiler.compile(value)
    vm.run(compiled)
  }
}
