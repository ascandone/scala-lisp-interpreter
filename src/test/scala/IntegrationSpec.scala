import compiler.Compiler
import org.scalatest.flatspec._
import org.scalatest.matchers._
import value.parser.Parser
import vm.Vm

class IntegrationSpec extends AnyFlatSpec with should.Matchers {


  behavior of "atomic values"
  they should "evaluate as themselves" in {
    expectVmToEvalAs("42", "42")
    expectVmToEvalAs("true", "true")
    expectVmToEvalAs("false", "false")
    expectVmToEvalAs("\"abc\"", "\"abc\"")
    expectVmToEvalAs("()", "()")
  }

  behavior of "+"
  it should "sum two numbers" in {
    expectVmToEvalAs("(+ 10 20)", "30")
  }

  behavior of ">"
  it should "behave as > operator" in {
    expectVmToEvalAs("(> 10 200)", "false")
    expectVmToEvalAs("(> 200 10)", "true")
    expectVmToEvalAs("(> 10 10)", "false")
  }

  behavior of "native operations"
  they should "work when nested" in {
    expectVmToEvalAs("(! (> (+ 100 1) 100))", "false")
  }

  // TODO should reify native operations

  behavior of "def expression"
  it should "bind value to globals" in {
    expectVmToEvalAs("(def x 42) x", "42")
    expectVmToEvalAs("(def x (+ 1 2)) x", "3")
    // TODO lambda
  }

  behavior of "if expression"
  it should "return the first argument when truthy" in {
    expectVmToEvalAs("(if true (+ 1 2) ())", "3")
    expectVmToEvalAs("(if 0 (+ 1 2) ())", "3")
    expectVmToEvalAs("(if 42 (+ 1 2) ())", "3")
  }

  it should "return the second argument when not truthy" in {
    expectVmToEvalAs("""(if false (+ 1 2) "nope")""", """"nope"""")
    expectVmToEvalAs("""(if () (+ 1 2) "nope")""", """"nope"""")
  }

  it should "work with complex expressions" in {
    // TODO more granular tests
    expectVmToEvalAs(
      """
    (if (! (> 10 200))
    "ok"
    "fail")
    """,
      """"ok""""
    )

    expectVmToEvalAs(
      """
      ()
      (if true
        "ok"
        "fail")
    """,
      """"ok""""
    )

    expectVmToEvalAs(
      """
      (def x false)

      (if (! x)
        "ok"
        "fail")
    """,
      """"ok""""
    )

    expectVmToEvalAs(
      """
       (def x false)

       (if (! x)
        (+ 1 (+ 1 10))
        "fail")
    """,
      "12"
    )
  }


  behavior of "abstraction and application"
  they should "work with no arguments" in {
    expectVmToEvalAs("""((lambda () "ok"))""", """"ok"""")
  }

  they should "work with two arguments" in {
    expectVmToEvalAs("""((lambda (a b) (+ a b)) 100 200)""", "300")
  }

  they should "resolve lambda saved in def" in {
    expectVmToEvalAs("""(def f (lambda () "ok")) (f)""", """"ok"""")
  }


  def expectVmToEvalAs(str: java.lang.String, expected: java.lang.String): Unit = {
    val parsed = Parser.run(str).get
    val compiled = Compiler.compile(parsed)

    val result = Vm.run(compiled)

    val parsedExpected = Parser.run(expected).get.head

    result should be(parsedExpected)
  }
}
