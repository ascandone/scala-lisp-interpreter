package cli

import interpreter.Interpreter

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {
  private val interpreter = new Interpreter().loadPrelude()

  def main(args: Array[String]): Unit =
    loop()

  @tailrec
  private def loop(): Unit = {
    print(">> ")
    val line = readLine()
    val retValue = interpreter.parseEval(line)
    println(retValue.show)
    loop()
  }
}
