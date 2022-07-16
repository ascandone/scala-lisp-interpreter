import interpreter.Interpreter
import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._
import vm.opcode.OpCode

class IntegrationSpec extends AnyFlatSpec with should.Matchers {
  behavior of "atomic values"
  they should "evaluate as themselves" in {
    "42" shouldEvalAs 42
    "true" shouldEvalAs true
    "false" shouldEvalAs false
    "\"abc\"" shouldEvalAs "abc"
    "()" shouldEvalAs Nil
  }

  behavior of "+"
  it should "sum two numbers" in {
    "(builtin/add 10 20)" shouldEvalAs 30
  }

  behavior of ">"
  it should "behave as > operator" in {
    "(builtin/greater-than 10 200)" shouldEvalAs false
    "(builtin/greater-than 200 10)" shouldEvalAs true
    "(builtin/greater-than 10 10)" shouldEvalAs false
  }

  behavior of "apply"
  it should "work with nonempty lists" in {
    "(def args '(10 20)) (builtin/apply (lambda* (x y) (builtin/add x y)) args)" shouldEvalAs 30
  }

  behavior of "native operations"
  they should "work when nested" in {
    "(builtin/not (builtin/greater-than (builtin/add 100 1) 100))" shouldEvalAs false
  }

  behavior of "def expression"
  it should "bind value to globals" in {
    "(def x 42) x" shouldEvalAs 42
    "(def x (builtin/add 1 2)) x" shouldEvalAs 3
    // TODO lambda
  }

  they should "resolve shadow each other" in {
    """
      (def x 0)

      (def f (lambda* () x))

      (def x 1)

      (f)
      """ shouldEvalAs 0
  }

  behavior of "if expression"
  it should "return the first argument when truthy" in {
    "(if true (builtin/add 1 2) ())" shouldEvalAs 3
    "(if 0 (builtin/add 1 2) ())" shouldEvalAs 3
    "(if 42 (builtin/add 1 2) ())" shouldEvalAs 3
  }

  it should "return the second argument when not truthy" in {
    """(if false (builtin/add 1 2) "nope")""" shouldEvalAs "nope"
    """(if () (builtin/add 1 2) "nope")""" shouldEvalAs "nope"
  }

  it should "handle nested expressions" in {
    """
      (if false
        'ignore
        (if true
          (if false 'ignore 'result)
          'ignore))
    """ shouldEvalAs Symbol("result")
  }


  it should "work with complex expressions" in {
    // TODO more granular tests
    """
  (if (builtin/not (builtin/greater-than 10 200))
    "ok"
    "fail")
  """ shouldEvalAs "ok"

    """
    ()
    (if true
      "ok"
      "fail")
  """ shouldEvalAs "ok"

    """
    (def x false)

    (if (builtin/not x)
      "ok"
      "fail")
  """ shouldEvalAs "ok"

    """
     (def x false)

     (if (builtin/not x)
      (builtin/add 1 (builtin/add 1 10))
      "fail")
  """ shouldEvalAs 12
  }


  behavior of "abstraction and application"
  they should "work with no arguments" in {
    """((lambda* () "ok"))""" shouldEvalAs "ok"
  }

  they should "work with two arguments" in {
    """((lambda* (a b) (builtin/add a b)) 100 200)""" shouldEvalAs 300
  }

  they should "resolve lambda saved in def" in {
    """(def f (lambda* () "ok")) (f)""" shouldEvalAs "ok"
  }

  they should "resolve local arguments" in {
    """
      (def f
        (lambda* () (do
          (def glob 42)
          glob)))

      (f)

      """ shouldEvalAs 42
  }

  they should "resolve be able to define global values" in {
    """
      (def f
        (lambda* (x)
          (def glob x)))

      (f 42)
      glob
      """ shouldEvalAs 42
  }

  they should "resolve be able to define global values, initialized with nil, before their call" in {
    """
      (def f
        (lambda* (x)
          (def glob x)))

      glob
      """ shouldEvalAs Nil
  }

  they should "handle nested frames" in {
    """
      (def one (lambda* () 100))
      (def two (lambda* () (one)))
      (def three (lambda* () (two)))
      (three)
      """ shouldEvalAs 100
  }

  they should "be able to read global env" in {
    """
    (def x 42)
    (def f (lambda* () x))
    (f)
      """ shouldEvalAs 42
  }

  they should "handle inner if expressions" in {
    """
      (def bool->string
        (lambda* (b)
          (do
            0 1 2
            (if b "true" "false"))))

     (bool->string true)
      """ shouldEvalAs "true"
  }

  they should "handle if expressions inside arguments list" in {

    """
      (def id (lambda* (ignored x) x))
      (id "ignore me" (if true "a" "b"))
      """ shouldEvalAs "a"
  }

  they should "handle functions passed as argument" in {
    """
    (def caller (lambda* (f) (f 10)))
    (caller (lambda* (n) (builtin/add n 1)))
    """ shouldEvalAs 11
  }

  they should "respect the arguments order" in {
    "((lambda* (x y) x) 0 1)" shouldEvalAs 0
    "((lambda* (x y) y) 0 1)" shouldEvalAs 1
  }

  they should "shadow parameter arguments" in {
    "((lambda* (x x) x) 0 1)" shouldEvalAs 1
  }

  they should "work with recursive bindings" in {
    val LIM = 4

    s"""
    (def f (lambda* (n)
      (if (builtin/greater-than n $LIM)
        n
        (f (builtin/add 1 n)))))

    (f 0)
    """ shouldEvalAs LIM + 1
  }

  they should "work with recursive bindings (without tail call)" in {
    val LIM = 4

    s"""
    (def f (lambda* (n)
      (if (builtin/greater-than n $LIM)
        n
        (builtin/add 1 (f (builtin/add 1 n))))))

    (f 0)
    """ shouldEvalAs 10
  }

  behavior of "tail call optimization"
  it should "allow simple recursion" in {
    val LIM = 1500

    s"""
    (def f (lambda* (n)
      (if (builtin/greater-than n $LIM)
        n
        (f (builtin/add 1 n)))))

    (f 0)
    """ shouldEvalAs (LIM + 1)
  }

  it should "allow simple recursion with swapped if branches" in {
    val LIM = 2000

    s"""
    (def f (lambda* (n)
      (if (builtin/not (builtin/greater-than n $LIM))
        (if (builtin/not true)
          100
          (f (builtin/add 1 n)))
        n)))

    (f 0)
    """ shouldEvalAs (LIM + 1)
  }

  it should "allow simple recursion with two params" in {
    val LIM = 1500

    s"""
    (def sum-n (lambda* (n acc)
      (if (builtin/greater-than n $LIM)
        acc
        (sum-n (builtin/add 1 n) (builtin/add acc n)))))

    (sum-n 0 0)
    """ shouldEvalAs (LIM * (LIM + 1) / 2)
  }

  it should "allow simple recursion with two params and one optional" in {
    val LIM = 4000

    s"""
    (def sum-n (lambda* (n &opt acc)
      (if (builtin/greater-than n $LIM)
        acc
        (sum-n (builtin/add 1 n) (builtin/add acc n)))))

    (sum-n 0 0)
    """ shouldEvalAs (LIM * (LIM + 1) / 2)
  }

  behavior of "closures"
  they should "have access to outer scope" in {
    """
    (def f
      (lambda* (a)
        (lambda* (b) (builtin/add a b))))

    ((f 10) 100)
    """ shouldEvalAs 110
  }

  they should "work with global and local def" in {
    """
      (def glob 20)

      (def f
        (lambda* (a) (do
          (def l 42)
          (lambda* (b)
            (builtin/add glob (builtin/add l (builtin/add a b)))))))

      ((f 10) 100)
    """ shouldEvalAs (10 + 42 + 100 + 20)
  }

  they should "handle &opt special args" in {
    "((lambda* (&opt a b) a) 0 1)" shouldEvalAs 0
    "((lambda* (&opt a b) b) 0 1)" shouldEvalAs 1
    "((lambda* (&opt a b) a) 0)" shouldEvalAs 0
    "((lambda* (&opt a b) b) 0)" shouldEvalAs Nil
    "((lambda* (&opt a b) a))" shouldEvalAs Nil
    "((lambda* (&opt a b) b))" shouldEvalAs Nil
  }

  they should "handle &rest special args" in {
    "((lambda* (&rest a) a))" shouldEvalAs Nil
    "((lambda* (&rest a) a) 0)" shouldEvalAs List.of(0)
    "((lambda* (&rest a) a) 0 1)" shouldEvalAs List.of(0, 1)
  }

  they should "handle regular arg mixed with &opt and &rest" in {
    "((lambda* (x &opt o &rest r) o) 0)" shouldEvalAs Nil
    "((lambda* (x &opt o &rest r) o) 0 1)" shouldEvalAs 1

    "((lambda* (x &opt o &rest r) r) 0 1)" shouldEvalAs Nil
    "((lambda* (x &opt o &rest r) r) 0 1 2)" shouldEvalAs List.of(2)
    "((lambda* (x &opt o &rest r) r) 0 1 2 3)" shouldEvalAs List.of(2, 3)
  }

  behavior of "macros"
  they should "be evaluated as nil when defined" in {
    "(defmacro mac () ())" shouldEvalAs Nil
  }

  they should "be expanded when returning literals" in {
    "(defmacro mac () 42) (mac)" shouldEvalAs 42
  }

  they should "be expanded when returning expressions" in {
    "(def x 42) (defmacro mac () 'x) (mac)" shouldEvalAs 42
  }

  they should "have access to arguments" in {
    "(defmacro mac (x y) x) (mac 42 1)" shouldEvalAs 42
  }

  they should "have access to unevaluated version of arguments" in {
    """
      (def x 42)
      (defmacro prevent-crash (x) (builtin/first x))
      (prevent-crash (x "this should crash"))
    """ shouldEvalAs 42
  }

  they should "have be able to return quoted version of args" in {
    """
      (defmacro prevent-crash (lst)
        (builtin/cons 'quote
          (builtin/cons (builtin/first lst)
            ())))

      (prevent-crash (f "this should crash"))
     """ shouldEvalAs Symbol("f")
  }

  they should "access global scope" in {
    "(def x 42) (defmacro mac () x) (mac)" shouldEvalAs 42
  }

  implicit class StringAssertions(val source: java.lang.String) {
    def shouldEvalAs(expected: Value[OpCode]): Unit = {
      val result = Interpreter.parseRun(source)
      result should be(expected)
    }
  }
}

class IntegrationLibSpec extends AnyFlatSpec with should.Matchers {
  it should "have a list function" in {
    "(list)" shouldEvalAs Nil
    "(list 1 2 3)" shouldEvalAs List.of(1, 2, 3)
  }

  it should "have a concat function" in {
    "(concat)" shouldEvalAs Nil
    "(concat '(1 2) nil '(3 4))" shouldEvalAs List.of(1, 2, 3, 4)
  }

  it should "have a map function" in {
    "(map nil (lambda (x) (+ 100 x)))" shouldEvalAs Nil
    "(map (list 1 2 3) (lambda (x) (+ 100 x)))" shouldEvalAs List.of(101, 102, 103)
  }

  behavior of "backquote macro"
  it should "behave as quote when used alone" in {
    "`1" shouldEvalAs 1
    "`a" shouldEvalAs Symbol("a")
    "`(1 a)" shouldEvalAs List.of(1, Symbol("a"))
  }

  it should "handle unquoting" in {
    "`,42" shouldEvalAs 42
    "(def x 42) `,x" shouldEvalAs 42
    "(def x 42) `(a ,x)" shouldEvalAs List.of(Symbol("a"), 42)
    "(def x 42) `(a (y ,x))" shouldEvalAs List.of(Symbol("a"), List.of(Symbol("y"), 42))
  }

  it should "handle unquote splicing" in {
    "`(1 2 ,@'(3 4))" shouldEvalAs List.of(1, 2, 3, 4)
    "(def x (list 2 3)) `(1 ,@x 4 5)" shouldEvalAs List.of(1, 2, 3, 4, 5)
  }

  behavior of "macros"
  it should "have a let1 macro" in {
    "(let1 (x (+ 10 20)) (+ 100 x))" shouldEvalAs 10 + 20 + 100
  }

  it should "have a let macro" in {
    "(let () 0)" shouldEvalAs 0
    "(let ((x 0)) x)" shouldEvalAs 0
    """
        (let ((a "a")
              (b (list a "b")))
          (cons "init" b))
      """ shouldEvalAs List.of("init", "a", "b")
  }

  behavior of "and macro"
  it should "return true when no args are passed" in {
    "(and)" shouldEvalAs true
  }

  it should "return the value when only one arg is passed" in {
    "(and true)" shouldEvalAs true
    "(and false)" shouldEvalAs false
    "(and 42)" shouldEvalAs 42
  }

  it should "work with multiple args" in {
    "(and true true)" shouldEvalAs true
    "(and true false)" shouldEvalAs false
    "(and false true)" shouldEvalAs false
    "(and false false)" shouldEvalAs false
    "(and true true false)" shouldEvalAs false
    "(and true true true)" shouldEvalAs true
  }

  it should "perform short-circuit evaluation with multiple args" in {
    "(and false (builtin/panic \"panic\"))" shouldEvalAs false
    "(and true true false (builtin/panic \"panic\"))" shouldEvalAs false
  }

  behavior of "or macro"
  it should "return true when no args are passed" in {
    "(or)" shouldEvalAs true
  }

  it should "return the value when only one arg is passed" in {
    "(or true)" shouldEvalAs true
    "(or false)" shouldEvalAs false
    "(or 42)" shouldEvalAs 42
  }

  it should "work with multiple args" in {
    "(or true true)" shouldEvalAs true
    "(or true false)" shouldEvalAs true
    "(or false true)" shouldEvalAs true
    "(or false false)" shouldEvalAs false
    "(or false false false)" shouldEvalAs false
    "(or true true false)" shouldEvalAs true
    "(or true true true)" shouldEvalAs true
  }

  it should "perform short-circuit evaluation with multiple args" in {
    "(or true (builtin/panic \"panic\"))" shouldEvalAs true
    "(or false false true (builtin/panic \"panic\"))" shouldEvalAs true
  }

  behavior of "-> macro"
  it should "return the first argument when no ops are passed" in {
    "(-> 42)" shouldEvalAs 42
  }

  it should "apply the first function" in {
    "(-> 42 (+ 100))" shouldEvalAs 142
  }

  it should "apply apply the function in the right order" in {
    "(-> 0 (> 100))" shouldEvalAs false
    "(-> 100 (> 0))" shouldEvalAs true
  }

  it should "handle multiple forms" in {
    "(-> 100 (+ 10) (+ 20) (+ 30))" shouldEvalAs (100 + 10 + 20 + 30)
  }

  behavior of "cond macro"
  it should "return nil when no args are passed" in {
    "(cond)" shouldEvalAs Nil
  }

  it should "return return the first true clause" in {
    "(cond (true 0))" shouldEvalAs 0
    "(cond (true 0) (true 42))" shouldEvalAs 0
    "(cond (false 0) (true 42))" shouldEvalAs 42
  }

  it should "return return nil when no true clauses are passed" in {
    "(cond (false 42))" shouldEvalAs Nil
  }

  it should "short-circuit the clauses" in {
    "(cond (true 0) ((builtin/panic \"panic\") 1))" shouldEvalAs 0
    "(cond (true 0) (true (builtin/panic \"panic\")))" shouldEvalAs 0
  }

  implicit class StringAssertions(val source: java.lang.String) {
    def shouldEvalAs(expected: Value[OpCode]): Unit = {
      val result = Interpreter.parseRun(source, loadPrelude = true)
      result should be(expected)
    }
  }
}
