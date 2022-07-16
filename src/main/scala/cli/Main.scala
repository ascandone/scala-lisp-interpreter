package cli

import compiler.CompilationError
import interpreter.Interpreter
import vm.RuntimeError

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
    try {
      print(">> ")
      val line = readLine()
      val retValue = interpreter.parseEval(line)
      println(retValue.show)
    } catch {
      case e: CompilationError => println(s"Compilation error: ${e.message}")
      case e: RuntimeError => println(e.message)
    }

    repl()
  }
}
