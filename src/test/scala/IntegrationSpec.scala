import interpreter.Interpreter
import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._
import vm.opcode.OpCode

class IntegrationSpec extends AnyFlatSpec with should.Matchers {
  behavior of "atomic values"
  they should "evaluate as themselves" in {
    expectVmToEvalAs("42", 42)
    expectVmToEvalAs("true", true)
    expectVmToEvalAs("false", false)
    expectVmToEvalAs("\"abc\"", "abc")
    expectVmToEvalAs("()", Nil)
  }

  behavior of "+"
  it should "sum two numbers" in {
    expectVmToEvalAs("(builtin/add 10 20)", 30)
  }

  behavior of ">"
  it should "behave as > operator" in {
    expectVmToEvalAs("(builtin/greater-than 10 200)", false)
    expectVmToEvalAs("(builtin/greater-than 200 10)", true)
    expectVmToEvalAs("(builtin/greater-than 10 10)", false)
  }

  behavior of "apply"
  it should "work with nonempty lists" in {
    expectVmToEvalAs("(def args '(10 20)) (builtin/apply (lambda* (x y) (builtin/add x y)) args)", 30)
  }

  behavior of "native operations"
  they should "work when nested" in {
    expectVmToEvalAs("(builtin/not (builtin/greater-than (builtin/add 100 1) 100))", false)
  }

  behavior of "def expression"
  it should "bind value to globals" in {
    expectVmToEvalAs("(def x 42) x", 42)
    expectVmToEvalAs("(def x (builtin/add 1 2)) x", 3)
    // TODO lambda
  }

  they should "resolve shadow each other" in {
    expectVmToEvalAs(
      """
        (def x 0)

        (def f (lambda* () x))

        (def x 1)

        (f)
        """, 0)
  }

  behavior of "if expression"
  it should "return the first argument when truthy" in {
    expectVmToEvalAs("(if true (builtin/add 1 2) ())", 3)
    expectVmToEvalAs("(if 0 (builtin/add 1 2) ())", 3)
    expectVmToEvalAs("(if 42 (builtin/add 1 2) ())", 3)
  }

  it should "return the second argument when not truthy" in {
    expectVmToEvalAs("""(if false (builtin/add 1 2) "nope")""", "nope")
    expectVmToEvalAs("""(if () (builtin/add 1 2) "nope")""", "nope")
  }

  it should "handle nested expressions" in {
    expectVmToEvalAs(
      """
        (if false
          'ignore
          (if true
            (if false 'ignore 'result)
            'ignore))
      """, Symbol("result"))
  }


  it should "work with complex expressions" in {
    // TODO more granular tests
    expectVmToEvalAs(
      """
    (if (builtin/not (builtin/greater-than 10 200))
    "ok"
    "fail")
    """,
      "ok"
    )

    expectVmToEvalAs(
      """
      ()
      (if true
        "ok"
        "fail")
    """,
      "ok"
    )

    expectVmToEvalAs(
      """
      (def x false)

      (if (builtin/not x)
        "ok"
        "fail")
    """,
      "ok"
    )

    expectVmToEvalAs(
      """
       (def x false)

       (if (builtin/not x)
        (builtin/add 1 (builtin/add 1 10))
        "fail")
    """,
      12
    )
  }


  behavior of "abstraction and application"
  they should "work with no arguments" in {
    expectVmToEvalAs("""((lambda* () "ok"))""", "ok")
  }

  they should "work with two arguments" in {
    expectVmToEvalAs("""((lambda* (a b) (builtin/add a b)) 100 200)""", 300)
  }

  they should "resolve lambda saved in def" in {
    expectVmToEvalAs("""(def f (lambda* () "ok")) (f)""", "ok")
  }

  they should "resolve local arguments" in {
    expectVmToEvalAs(
      """
        (def f
          (lambda* () (do
            (def glob 42)
            glob)))

        (f)

        """, 42)
  }

  they should "resolve be able to define global values" in {
    expectVmToEvalAs(
      """
        (def f
          (lambda* (x)
            (def glob x)))

        (f 42)
        glob
        """, 42)
  }

  they should "resolve be able to define global values, initialized with nil, before their call" in {
    expectVmToEvalAs(
      """
        (def f
          (lambda* (x)
            (def glob x)))

        glob
        """, Nil)
  }

  they should "handle nested frames" in {
    expectVmToEvalAs(
      """
        (def one (lambda* () 100))
        (def two (lambda* () (one)))
        (def three (lambda* () (two)))
        (three)
        """, 100)
  }

  they should "be able to read global env" in {
    expectVmToEvalAs(
      """
      (def x 42)
      (def f (lambda* () x))
      (f)
        """, 42)
  }

  they should "handle inner if expressions" in {
    expectVmToEvalAs(
      """
        (def bool->string
          (lambda* (b)
            (do
              0 1 2
              (if b "true" "false"))))

       (bool->string true)
        """, "true")
  }

  they should "handle if expressions inside arguments list" in {
    expectVmToEvalAs(
      """
      (def id (lambda* (ignored x) x))
      (id "ignore me" (if true "a" "b"))
      """, "a")
  }

  they should "handle functions passed as argument" in {
    expectVmToEvalAs(
      """
      (def caller (lambda* (f) (f 10)))
      (caller (lambda* (n) (builtin/add n 1)))
      """, 11)
  }

  they should "respect the arguments order" in {
    expectVmToEvalAs("((lambda* (x y) x) 0 1)", 0)
    expectVmToEvalAs("((lambda* (x y) y) 0 1)", 1)
  }

  they should "shadow parameter arguments" in {
    expectVmToEvalAs("((lambda* (x x) x) 0 1)", 1)
  }

  they should "work with recursive bindings" in {
    val LIM = 4

    expectVmToEvalAs(
      s"""
      (def f (lambda* (n)
        (if (builtin/greater-than n $LIM)
          n
          (f (builtin/add 1 n)))))

      (f 0)
      """, LIM + 1)
  }

  they should "work with recursive bindings (without tail call)" in {
    val LIM = 4

    expectVmToEvalAs(
      s"""
      (def f (lambda* (n)
        (if (builtin/greater-than n $LIM)
          n
          (builtin/add 1 (f (builtin/add 1 n))))))

      (f 0)
      """, 10)
  }

  behavior of "tail call optimization"
  it should "allow simple recursion" in {
    val LIM = 1500

    expectVmToEvalAs(
      s"""
      (def f (lambda* (n)
        (if (builtin/greater-than n $LIM)
          n
          (f (builtin/add 1 n)))))

      (f 0)
      """, LIM + 1)
  }

  it should "allow simple recursion with swapped if branches" in {
    val LIM = 2000

    expectVmToEvalAs(
      s"""
      (def f (lambda* (n)
        (if (builtin/not (builtin/greater-than n $LIM))
          (if (builtin/not true)
            100
            (f (builtin/add 1 n)))
          n)))

      (f 0)
      """, LIM + 1)
  }

  it should "allow simple recursion with two params" in {
    val LIM = 1500

    expectVmToEvalAs(
      s"""
      (def sum-n (lambda* (n acc)
        (if (builtin/greater-than n $LIM)
          acc
          (sum-n (builtin/add 1 n) (builtin/add acc n)))))

      (sum-n 0 0)
      """, LIM * (LIM + 1) / 2)
  }

  it should "allow simple recursion with two params and one optional" in {
    val LIM = 4000

    expectVmToEvalAs(
      s"""
      (def sum-n (lambda* (n &opt acc)
        (if (builtin/greater-than n $LIM)
          acc
          (sum-n (builtin/add 1 n) (builtin/add acc n)))))

      (sum-n 0 0)
      """, LIM * (LIM + 1) / 2)
  }

  behavior of "closures"
  they should "have access to outer scope" in {
    expectVmToEvalAs(
      """
      (def f
        (lambda* (a)
          (lambda* (b) (builtin/add a b))))

      ((f 10) 100)
      """, 110)
  }

  they should "work with global and local def" in {
    expectVmToEvalAs(
      """
        (def glob 20)

        (def f
          (lambda* (a) (do
            (def l 42)
            (lambda* (b)
              (builtin/add glob (builtin/add l (builtin/add a b)))))))

        ((f 10) 100)
      """, 10 + 42 + 100 + 20)
  }

  they should "handle &opt special args" in {
    expectVmToEvalAs("((lambda* (&opt a b) a) 0 1)", 0)
    expectVmToEvalAs("((lambda* (&opt a b) b) 0 1)", 1)
    expectVmToEvalAs("((lambda* (&opt a b) a) 0)", 0)
    expectVmToEvalAs("((lambda* (&opt a b) b) 0)", Nil)
    expectVmToEvalAs("((lambda* (&opt a b) a))", Nil)
    expectVmToEvalAs("((lambda* (&opt a b) b))", Nil)
  }

  they should "handle &rest special args" in {
    expectVmToEvalAs("((lambda* (&rest a) a))", Nil)
    expectVmToEvalAs("((lambda* (&rest a) a) 0)", List.of(0))
    expectVmToEvalAs("((lambda* (&rest a) a) 0 1)", List.of(0, 1))
  }

  they should "handle regular arg mixed with &opt and &rest" in {
    expectVmToEvalAs("((lambda* (x &opt o &rest r) o) 0)", Nil)
    expectVmToEvalAs("((lambda* (x &opt o &rest r) o) 0 1)", 1)

    expectVmToEvalAs("((lambda* (x &opt o &rest r) r) 0 1)", Nil)
    expectVmToEvalAs("((lambda* (x &opt o &rest r) r) 0 1 2)", List.of(2))
    expectVmToEvalAs("((lambda* (x &opt o &rest r) r) 0 1 2 3)", List.of(2, 3))
  }

  behavior of "macros"
  they should "be evaluated as nil when defined" in {
    expectVmToEvalAs("(defmacro mac () ())", Nil)
  }

  they should "be expanded when returning literals" in {
    expectVmToEvalAs("(defmacro mac () 42) (mac)", 42)
  }

  they should "be expanded when returning expressions" in {
    expectVmToEvalAs("(def x 42) (defmacro mac () 'x) (mac)", 42)
  }

  they should "have access to arguments" in {
    expectVmToEvalAs("(defmacro mac (x y) x) (mac 42 1)", 42)
  }

  they should "have access to unevaluated version of arguments" in {
    expectVmToEvalAs(
      "(def x 42) (defmacro prevent-crash (x) (builtin/first x)) (prevent-crash (x \"this should crash\"))",
      42,
    )
  }

  they should "have be able to return quoted version of args" in {
    expectVmToEvalAs(
      """
        (defmacro prevent-crash (lst)
          (builtin/cons 'quote
            (builtin/cons (builtin/first lst)
              ())))

        (prevent-crash (f "this should crash"))
       """,
      Symbol("f")
    )
  }

  they should "access global scope" in {
    expectVmToEvalAs("(def x 42) (defmacro mac () x) (mac)", 42)
  }


  def expectVmToEvalAs(str: java.lang.String, expected: Value[OpCode]): Unit = {
    val result = Interpreter.parseRun(str)
    result should be(expected)
  }
}

class IntegrationLibSpec extends AnyFlatSpec with should.Matchers {
  it should "have a list function" in {
    expectVmToEvalAs("(list)", Nil)
    expectVmToEvalAs("(list 1 2 3)", List.of(1, 2, 3))
  }

  it should "have a concat function" in {
    expectVmToEvalAs("(concat)", Nil)
    expectVmToEvalAs("(concat '(1 2) nil '(3 4))", List.of(1, 2, 3, 4))
  }

  it should "have a map function" in {
    expectVmToEvalAs("(map nil (lambda (x) (+ 100 x)))", Nil)
    expectVmToEvalAs("(map (list 1 2 3) (lambda (x) (+ 100 x)))", List.of(101, 102, 103))
  }

  behavior of "backquote macro"
  it should "behave as quote when used alone" in {
    expectVmToEvalAs("`1", 1)
    expectVmToEvalAs("`a", Symbol("a"))
    expectVmToEvalAs("`(1 a)", List.of(1, Symbol("a")))
  }

  it should "handle unquoting" in {
    expectVmToEvalAs("`,42", 42)
    expectVmToEvalAs("(def x 42) `,x", 42)
    expectVmToEvalAs("(def x 42) `(a ,x)", List.of(Symbol("a"), 42))
    expectVmToEvalAs("(def x 42) `(a (y ,x))", List.of(Symbol("a"), List.of(Symbol("y"), 42)))
  }

  it should "handle unquote splicing" in {
    expectVmToEvalAs("`(1 2 ,@'(3 4))", List.of(1, 2, 3, 4))
    expectVmToEvalAs("(def x (list 2 3)) `(1 ,@x 4 5)", List.of(1, 2, 3, 4, 5))
  }

  behavior of "macros"
  it should "have a let1 macro" in {
    expectVmToEvalAs("(let1 (x (+ 10 20)) (+ 100 x))", 10 + 20 + 100)
  }

  it should "have a let macro" in {
    expectVmToEvalAs("(let () 0)", 0)
    expectVmToEvalAs("(let ((x 0)) x)", 0)
    expectVmToEvalAs(
      """
          (let ((a "a")
                (b (list a "b")))
            (cons "init" b))
        """, List.of("init", "a", "b"))
  }

  behavior of "and macro"
  it should "return true when no args are passed" in {
    expectVmToEvalAs("(and)", true)
  }

  it should "return the value when only one arg is passed" in {
    expectVmToEvalAs("(and true)", true)
    expectVmToEvalAs("(and false)", false)
    expectVmToEvalAs("(and 42)", 42)
  }

  it should "work with multiple args" in {
    expectVmToEvalAs("(and true true)", true)
    expectVmToEvalAs("(and true false)", false)
    expectVmToEvalAs("(and false true)", false)
    expectVmToEvalAs("(and false false)", false)
    expectVmToEvalAs("(and true true false)", false)
    expectVmToEvalAs("(and true true true)", true)
  }

  it should "perform short-circuit evaluation with multiple args" in {
    expectVmToEvalAs("(and false (builtin/panic \"panic\"))", false)
    expectVmToEvalAs("(and true true false (builtin/panic \"panic\"))", false)
  }

  behavior of "or macro"
  it should "return true when no args are passed" in {
    expectVmToEvalAs("(or)", true)
  }

  it should "return the value when only one arg is passed" in {
    expectVmToEvalAs("(or true)", true)
    expectVmToEvalAs("(or false)", false)
    expectVmToEvalAs("(or 42)", 42)
  }

  it should "work with multiple args" in {
    expectVmToEvalAs("(or true true)", true)
    expectVmToEvalAs("(or true false)", true)
    expectVmToEvalAs("(or false true)", true)
    expectVmToEvalAs("(or false false)", false)
    expectVmToEvalAs("(or false false false)", false)
    expectVmToEvalAs("(or true true false)", true)
    expectVmToEvalAs("(or true true true)", true)
  }

  it should "perform short-circuit evaluation with multiple args" in {
    expectVmToEvalAs("(or true (builtin/panic \"panic\"))", true)
    expectVmToEvalAs("(or false false true (builtin/panic \"panic\"))", true)
  }

  def expectVmToEvalAs(str: java.lang.String, expected: Value[OpCode]): Unit = {
    val result = Interpreter.parseRun(str, loadPrelude = true)
    result should be(expected)
  }
}
