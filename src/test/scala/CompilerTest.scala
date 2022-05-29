import compiler._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._
import value.parser.Parser
import vm._

class CompilerTest extends AnyFlatSpec with should.Matchers {

  it should "compile constants" in {
    testCompileAs("42",
      Array(
        Push(Number(42))
      )
    )

    testCompileAs("\"hello\"",
      Array(
        Push(String("hello"))
      )
    )

    testCompileAs("()",
      Array(
        Push(Value.nil)
      )
    )

    testCompileAs("true",
      Array(
        Push(Symbol("true"))
      )
    )

    testCompileAs("false",
      Array(
        Push(Symbol("false"))
      )
    )
  }


  def testCompileAs(str: java.lang.String, instructions: Array[OpCode]): Unit = {
    val parsed = Parser.run(str).get
    val compiled = Compiler.compile(parsed)
    compiled should be(instructions)
  }
}
