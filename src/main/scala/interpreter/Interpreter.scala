package interpreter

import compiler.Compiler
import value.Value
import value.parser.Parser
import vm._

object Interpreter {
  def parseRun(src: java.lang.String): Value[OpCode] = new Interpreter().parseEval(src)
}

class Interpreter {
  private val vm = new Vm
  private val compiler = new Compiler(vm)

  def parseEval(src: java.lang.String): Value[OpCode] = {
    val values = Parser.run(src).get
    eval(values)
  }

  def eval(values: scala.List[Value[Nothing]]): Value[OpCode] = values.map(value => {
    val compiled = compiler.compile(scala.List(value))
    vm.run(compiled)
  }).last
}
