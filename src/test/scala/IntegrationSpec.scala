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

  they should "resolve shadow each other" in {
    expectVmToEvalAs(
      """
        (def x 0)

        (def f (lambda () x))

        (def x 1)

        (f)
        """, "0")
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

  they should "resolve local arguments" in {
    expectVmToEvalAs(
      """
        (def f
          (lambda () (do
            (def glob 42)
            glob)))

        (f)

        """, "42")
  }

  they should "resolve be able to define global values" in {
    expectVmToEvalAs(
      """
        (def f
          (lambda (x)
            (def glob x)))

        (f 42)
        glob
        """, "42")
  }

  they should "resolve be able to define global values, initialized with nil, before their call" in {
    expectVmToEvalAs(
      """
        (def f
          (lambda (x)
            (def glob x)))

        glob
        """, "()")
  }

  they should "handle nested frames" in {
    expectVmToEvalAs(
      """
        (def one (lambda () 100))
        (def two (lambda () (one)))
        (def three (lambda () (two)))
        (three)
        """, "100")
  }

  they should "be able to read global env" in {
    expectVmToEvalAs(
      """
      (def x 42)
      (def f (lambda () x))
      (f)
        """, "42")
  }

  they should "handle inner if expressions" in {
    expectVmToEvalAs(
      """
        (def bool->string
          (lambda (b)
            (do
              0 1 2
              (if b "true" "false"))))

       (bool->string true)
        """, "\"true\"")
  }

  they should "handle if expressions inside arguments list" in {
    expectVmToEvalAs(
      """
      (def id (lambda (ignored x) x))
      (id "ignore me" (if true "a" "b"))
      """, "\"a\"")
  }

  they should "handle functions passed as argument" in {
    expectVmToEvalAs(
      """
      (def caller (lambda (f) (f 10)))
      (caller (lambda (n) (+ n 1)))
      """, "11")
  }

  they should "respect the arguments order" in {
    expectVmToEvalAs("((lambda (x y) x) 0 1)", "0")
    expectVmToEvalAs("((lambda (x y) y) 0 1)", "1")
  }

  they should "shadow parameter arguments" in {
    expectVmToEvalAs("((lambda (x x) x) 0 1)", "1")
  }

  they should "work with recursive bindings" in {
    val LIM = 4

    expectVmToEvalAs(
      s"""
      (def f (lambda (n)
        (if (> n $LIM)
          n
          (f (+ 1 n)))))

      (f 0)
      """, (LIM + 1).toString)
  }

  behavior of "closures"
  they should "have access to outer scope" in {
    expectVmToEvalAs(
      """
      (def f
        (lambda (a)
          (lambda (b) (+ a b))))

      ((f 10) 100)
      """, "110")
  }

  they should "work with global and local def" in {
    expectVmToEvalAs(
      """
        (def glob 20)

        (def f
          (lambda (a) (do
            (def l 42)
            (lambda (b)
              (+ glob (+ l (+ a b)))))))

        ((f 10) 100)
      """, (10 + 42 + 100 + 20).toString)
  }

  they should "handle &opt special args" in {
    expectVmToEvalAs("((lambda (&opt a b) a) 0 1)", "0")
    expectVmToEvalAs("((lambda (&opt a b) b) 0 1)", "1")
    expectVmToEvalAs("((lambda (&opt a b) a) 0)", "0")
    expectVmToEvalAs("((lambda (&opt a b) b) 0)", "()")
    expectVmToEvalAs("((lambda (&opt a b) a))", "()")
    expectVmToEvalAs("((lambda (&opt a b) b))", "()")
  }

  they should "handle &rest special args" in {
    expectVmToEvalAs("((lambda (&rest a) a))", "()")
    expectVmToEvalAs("((lambda (&rest a) a) 0)", "(0)")
    expectVmToEvalAs("((lambda (&rest a) a) 0 1)", "(0 1)")
  }

  they should "handle regular arg mixed with &opt and &rest" in {
    expectVmToEvalAs("((lambda (x &opt o &rest r) o) 0)", "()")
    expectVmToEvalAs("((lambda (x &opt o &rest r) o) 0 1)", "1")

    expectVmToEvalAs("((lambda (x &opt o &rest r) r) 0 1)", "()")
    expectVmToEvalAs("((lambda (x &opt o &rest r) r) 0 1 2)", "(2)")
    expectVmToEvalAs("((lambda (x &opt o &rest r) r) 0 1 2 3)", "(2 3)")
  }


  def expectVmToEvalAs(str: java.lang.String, expected: java.lang.String): Unit = {
    val parsed = Parser.run(str).get
    val compiled = Compiler.compile(parsed)

    val result = Vm.runOnce(compiled)

    val parsedExpected = Parser.run(expected).get.head

    result should be(parsedExpected)
  }
}
