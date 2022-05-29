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

  it should "compile empty do blocks" in {
    testCompileAs("(do)",
      Array(
        Push(Value.nil)
      )
    )
  }

  it should "compile singleton do blocks" in {
    testCompileAs("(do 42)",
      Array(
        Push(Number(42)),
      )
    )
  }

  it should "compile do blocks with many values" in {
    testCompileAs("(do 1 2 3)",
      Array(
        Push(Number(1)),
        Pop,
        Push(Number(2)),
        Pop,
        Push(Number(3)),
      )
    )
  }

  it should "treat multiple values as a do block" in {
    testCompileAs("1 2 3",
      Array(
        Push(Number(1)),
        Pop,
        Push(Number(2)),
        Pop,
        Push(Number(3)),
      )
    )
  }

  it should "compile +" in {
    testCompileAs("(+ 1 2)",
      Array(
        Push(Number(1)),
        Push(Number(2)),
        Op2(Add),
      )
    )
  }

  it should "compile >" in {
    testCompileAs("(> 1 2)",
      Array(
        Push(Number(1)),
        Push(Number(2)),
        Op2(GreaterThan),
      )
    )
  }

  def testCompileAs(str: java.lang.String, instructions: Array[OpCode]): Unit = {
    val parsed = Parser.run(str).get
    val compiled = Compiler.compile(parsed)
    compiled should be(instructions)
  }
}
