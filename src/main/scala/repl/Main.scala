package repl

import compiler._
import value.parser.Parser
import vm.Vm

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {
  private val compiler = new Compiler()
  private val vm = new Vm

  def main(args: Array[String]): Unit = {
    loop()
  }

  @tailrec
  private def loop(): Unit = {
    print(">> ")
    val line = readLine()
    val parsed = Parser.run(line).get
    val compiled = compiler.compile(parsed)
    val retValue = vm.run(compiled)
    println(retValue.show)
    loop()
  }
}