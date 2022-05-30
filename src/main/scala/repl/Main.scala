package repl

import value.parser.Parser
import vm.Vm

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {
    loop()
  }

  @tailrec
  private def loop(): Unit = {
    print(">> ")
    val line = readLine()
    val parsed = Parser.run(line).get
    val compiled = compiler.Compiler.compile(parsed)
    val retValue = Vm.run(compiled)
    println(retValue.show)
    loop()
  }
}
