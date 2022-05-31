package cli

import interpreter.Interpreter

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine

object Main {
  private val interpreter = new Interpreter().loadPrelude()

  def main(args: Array[String]): Unit = {
    args.length match {
      case 0 => repl()
      case _ => run(args(0))
    }
  }

  private def run(path: String): Unit = {
    val bufferedSource = Source.fromFile(path)
    val content = bufferedSource.getLines().toList.mkString("")
    bufferedSource.close()

    interpreter.parseEval(content)
  }

  @tailrec
  private def repl(): Unit = {
    print(">> ")
    val line = readLine()
    val retValue = interpreter.parseEval(line)
    println(retValue.show)
    repl()
  }
}
