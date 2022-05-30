package compiler

import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._
import value.parser.Parser
import vm._

class CompilerSpec extends AnyFlatSpec with should.Matchers {

  it should "compile constants" in {
    testCompileAs("42",
      Array(
        Push(42)
      )
    )

    testCompileAs("\"hello\"",
      Array(
        Push("hello")
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
        Push(42),
      )
    )
  }

  it should "compile do blocks with many values" in {
    testCompileAs("(do 1 2 3)",
      Array(
        Push(1),
        Pop,
        Push(2),
        Pop,
        Push(3),
      )
    )
  }

  it should "treat multiple values as a do block" in {
    testCompileAs("1 2 3",
      Array(
        Push(1),
        Pop,
        Push(2),
        Pop,
        Push(3),
      )
    )
  }

  it should "compile +" in {
    testCompileAs("(+ 1 2)",
      Array(
        Push(1),
        Push(2),
        Op2(Add),
      )
    )
  }

  it should "compile >" in {
    testCompileAs("(> 1 2)",
      Array(
        Push(1),
        Push(2),
        Op2(GreaterThan),
      )
    )
  }

  it should "compile not" in {
    testCompileAs("(! true)",
      Array(
        Push(true),
        Op1(Not),
      )
    )
  }

  it should "compile a if expression of values" in {
    testCompileAs("(if true \"when true\" \"when false\")",
      Array(
        /* 0 */ Push(true),
        /* 1 */ JumpIfNot(4),
        /* 2 */ Push("when true"),
        /* 3 */ Jump(5),
        /* 4 */ Push("when false"),
        /* 5 */
      )
    )
  }

  it should "compile a if expression of expressions" in {
    testCompileAs("(if (> 100 200) (+ 10 20) (! ()))",
      Array(
        /* 00 */ Push(100),
        /* 01 */ Push(200),
        /* 02 */ Op2(GreaterThan),
        /* 03 */ JumpIfNot(8),
        /* 04 */ Push(10), // if branch
        /* 05 */ Push(20),
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
      Push(10),
      SetGlobal(0),
      Pop,

      Push(20),
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
      Push(42),
    ))

    testCompileAs("(quote \"hello\")", Array(
      Push("hello"),
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

class ArgsCompilerSpec extends AnyFlatSpec with should.Matchers {
  it should "compile regular args" in {
    CompiledArgs() should be(new CompiledArgs())
    CompiledArgs("a") should be(new CompiledArgs(required = scala.List("a")))
    CompiledArgs("a", "b") should be(new CompiledArgs(required = scala.List("a", "b")))
  }

  it should "compile optional args" in {
    CompiledArgs("&opt") should be(new CompiledArgs())
    CompiledArgs("&opt", "a") should be(new CompiledArgs(optionals = scala.List("a")))
    CompiledArgs("&opt", "a", "b") should be(new CompiledArgs(optionals = scala.List("a", "b")))
  }

  it should "compile regular args mixed with optional" in {
    CompiledArgs("x", "&opt") should be(new CompiledArgs(
      required = scala.List("x")
    ))

    CompiledArgs("x", "&opt", "a") should be(new CompiledArgs(
      required = scala.List("x"),
      optionals = scala.List("a")
    ))

    CompiledArgs("x", "&opt", "a", "b") should be(new CompiledArgs(
      required = scala.List("x"),
      optionals = scala.List("a", "b")
    ))
  }
}
