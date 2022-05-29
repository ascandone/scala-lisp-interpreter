import compiler._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._
import value.parser.Parser
import vm._

class CompilerSpec extends AnyFlatSpec with should.Matchers {

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

  it should "compile not" in {
    testCompileAs("(! true)",
      Array(
        Push(Value.fromBool(true)),
        Op1(Not),
      )
    )
  }

  it should "compile a if expression of values" in {
    testCompileAs("(if true \"when true\" \"when false\")",
      Array(
        /* 0 */ Push(Value.fromBool(true)),
        /* 1 */ JumpIfNot(4),
        /* 2 */ Push(String("when true")),
        /* 3 */ Jump(5),
        /* 4 */ Push(String("when false")),
        /* 5 */
      )
    )
  }

  it should "compile a if expression of expressions" in {
    testCompileAs("(if (> 100 200) (+ 10 20) (! ()))",
      Array(
        /* 00 */ Push(Number(100)),
        /* 01 */ Push(Number(200)),
        /* 02 */ Op2(GreaterThan),
        /* 03 */ JumpIfNot(8),
        /* 04 */ Push(Number(10)), // if branch
        /* 05 */ Push(Number(20)),
        /* 06 */ Op2(Add),
        /* 07 */ Jump(10),
        /* 08 */ Push(List.of()), // else branch
        /* 09 */ Op1(Not), // else branch
        /* 10 */
      )
    )
  }

  it should "compile def expressions" in {
    testCompileAs("(def x 10) (def y 20) (+ x y)", Array(
      Push(Number(10)),
      SetGlobal(0),
      Pop,

      Push(Number(20)),
      SetGlobal(1),
      Pop,

      GetGlobal(0),
      GetGlobal(1),
      Op2(Add),
    ))
  }

  behavior of "quote special form"
  it should "be a noop with literals" in {
    testCompileAs("(quote 42)", Array(
      Push(Number(42)),
    ))

    testCompileAs("(quote \"hello\")", Array(
      Push(String("hello")),
    ))

    testCompileAs("(quote ())", Array(
      Push(List()),
    ))
  }

  it should "prevent symbols from evaluating" in {
    testCompileAs("(quote a)", Array(
      Push(Symbol("a")),
    ))
  }

  it should "prevent lists from evaluating" in {
    testCompileAs("(quote (a b))", Array(
      Push(List.of(Symbol("a"), Symbol("b"))),
    ))
  }

  def testCompileAs(str: java.lang.String, instructions: Array[OpCode]): Unit = {
    val parsed = Parser.run(str).get
    val compiled = Compiler.compile(parsed)
    compiled should be(instructions)
  }
}
