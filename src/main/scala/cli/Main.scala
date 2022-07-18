package cli

import compiler.CompilationError
import interpreter.Interpreter
import value.parser.ParsingError
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
  private def readBlock(acc: java.lang.String = ""): java.lang.String = {
    val line = readLine()
    line match {
      case ":}" => acc
      case _ => readBlock(acc + line)
    }
  }

  private def evalBlock(source: java.lang.String): Unit = {
    try {
      val retValue = interpreter.parseEval(source)
      println(retValue.show)
    } catch {
      case e: CompilationError => println(s"Compilation error: ${e.message}")
      case e: RuntimeError => println(e.message)
      case e: ParsingError => println(s"Parsing error: ${e.message}")
    }
  }

  @tailrec
  private def repl(): Unit = {
    print(">> ")
    val line = readLine()

    line match {
      case ":reload" => interpreter.loadPrelude()
      case ":{" =>
        val source = readBlock()
        evalBlock(source)
      case _ => evalBlock(line)
    }

    repl()
  }
}
