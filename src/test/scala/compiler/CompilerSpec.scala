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
    testCompileAs("(builtin/add 1 2)",
      Array(
        Push(1),
        Push(2),
        Builtin(Add),
      )
    )
  }

  it should "compile >" in {
    testCompileAs("(builtin/greater-than 1 2)",
      Array(
        Push(1),
        Push(2),
        Builtin(GreaterThan),
      )
    )
  }

  it should "compile not" in {
    testCompileAs("(builtin/not true)",
      Array(
        Push(true),
        Builtin(Not),
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
    testCompileAs("(if (builtin/greater-than 100 200) (builtin/add 10 20) (builtin/not ()))",
      Array(
        /* 00 */ Push(100),
        /* 01 */ Push(200),
        /* 02 */ Builtin(GreaterThan),
        /* 03 */ JumpIfNot(8),
        /* 04 */ Push(10), // if branch
        /* 05 */ Push(20),
        /* 06 */ Builtin(Add),
        /* 07 */ Jump(10),
        /* 08 */ Push(List.of()), // else branch
        /* 09 */ Builtin(Not), // else branch
        /* 10 */
      )
    )
  }

  it should "compile def expressions" in {
    testCompileAs("(def x 10) (def y 20) (builtin/add x y)", Array(
      Push(10),
      SetGlobal(0),
      Pop,

      Push(20),
      SetGlobal(1),
      Pop,

      GetGlobal(0),
      GetGlobal(1),
      Builtin(Add),
    ))
  }

  behavior of "quote special form"
  it should "be a noop with literals" in {
    testCompileAs("'42", Array(
      Push(42),
    ))

    testCompileAs("'\"hello\"", Array(
      Push("hello"),
    ))

    testCompileAs("'()", Array(
      Push(List()),
    ))
  }

  it should "prevent symbols from evaluating" in {
    testCompileAs("'a", Array(
      Push(Symbol("a")),
    ))
  }

  it should "prevent lists from evaluating" in {
    testCompileAs("'(a b)", Array(
      Push(List.of(Symbol("a"), Symbol("b"))),
    ))
  }

  behavior of "macros"
  they should "be evaluated as nil when defined" in {
    testCompileAs("(defmacro mac () ())", Array(
      Push(Nil),
    ))
  }

  they should "be expanded when returning literals" in {
    testCompileAs("(defmacro mac () 42) (mac)", Array(
      Push(Nil),
      Pop,
      Push(42),
    ))
  }

  they should "be expanded when returning expressions" in {
    testCompileAs("(def x 42) (defmacro mac () (quote x)) (mac)", Array(
      Push(42),
      SetGlobal(0),
      Pop,

      Push(Nil),
      Pop,

      GetGlobal(0),
    ))
  }

  they should "have access to arguments" in {
    testCompileAs("(defmacro mac (x y) x) (mac 42 1)", Array(
      Push(Nil),
      Pop,

      Push(42),
    ))
  }

  they should "prevent arguments to evaluate" in {
    testCompileAs("(defmacro prevent-crash (x) ()) (prevent-crash (+ \"this should crash\"))", Array(
      Push(Nil),
      Pop,

      Push(Nil),
    ))
  }

  they should "have access to unevaluated version of arguments" in {
    testCompileAs("(def x 42) (defmacro prevent-crash (x) (builtin/first x)) (prevent-crash (x \"this should crash\"))", Array(
      Push(42),
      SetGlobal(0),
      Pop,

      Push(Nil),
      Pop,

      GetGlobal(0),
    ))
  }

  they should "have be able to return quoted version of args" in {
    testCompileAs(
      """
        (defmacro prevent-crash (x)
          (builtin/cons 'quote
            (builtin/cons (builtin/first x)
              ())))

        (prevent-crash (+ "this should crash"))
       """,
      Array(
        Push(Nil),
        Pop,

        Push(Symbol("+")),
      )
    )
  }

  def testCompileAs(str: java.lang.String, instructions: Array[OpCode]): Unit = {
    val parsed = Parser.run(str).get
    val compiled = Compiler.compile(parsed)
    compiled should be(instructions)
  }
}

class ArgsCompilerSpec extends AnyFlatSpec with should.Matchers {
  it should "compile regular args" in {
    CompiledParams() should be(new CompiledParams())
    CompiledParams("a") should be(new CompiledParams(required = scala.List("a")))
    CompiledParams("a", "b") should be(new CompiledParams(required = scala.List("a", "b")))
  }

  it should "compile optional args" in {
    CompiledParams("&opt") should be(new CompiledParams())
    CompiledParams("&opt", "a") should be(new CompiledParams(optionals = scala.List("a")))
    CompiledParams("&opt", "a", "b") should be(new CompiledParams(optionals = scala.List("a", "b")))
  }

  it should "compile regular args mixed with optional" in {
    CompiledParams("x", "&opt") should be(new CompiledParams(
      required = scala.List("x")
    ))

    CompiledParams("x", "&opt", "a") should be(new CompiledParams(
      required = scala.List("x"),
      optionals = scala.List("a")
    ))

    CompiledParams("x", "&opt", "a", "b") should be(new CompiledParams(
      required = scala.List("x"),
      optionals = scala.List("a", "b")
    ))
  }

  it should "compile rest args" in {
    CompiledParams("&rest", "a") should be(new CompiledParams(rest = Some("a")))
  }

  it should "compile regular args mixed with rest" in {
    CompiledParams("x", "y", "&rest", "a") should be(new CompiledParams(
      required = scala.List("x", "y"),
      rest = Some("a"),
    ))
  }

  it should "compile regular args mixed with optional and rest" in {
    CompiledParams("x", "y", "&opt", "o1", "o2", "&rest", "a") should be(new CompiledParams(
      required = scala.List("x", "y"),
      optionals = scala.List("o1", "o2"),
      rest = Some("a"),
    ))
  }
}
