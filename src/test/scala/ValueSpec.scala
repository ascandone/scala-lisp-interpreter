import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._

class ValueSpec extends AnyFlatSpec with should.Matchers {
  it should "override toString" in {
    Number(0).show should be("0")
    String("hello").show should be("\"hello\"")
    Symbol("hello").show should be("hello")
    List.of().show should be("nil")
    List.of(
      Symbol("a"),
      List.of(),
      Symbol("c"),
    ).show should be("(a nil c)")
  }
}

class ArgumentsSpec extends AnyFlatSpec with should.Matchers {
  it should "parse required arguments" in {
    ArgumentsArity() parse scala.List() should be(
      Right(ArgumentsArity.ParsedArguments())
    )

    ArgumentsArity(required = 2) parse scala.List("a", "b") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a", "b"),
      ))
    )
  }

  it should "parse rest arguments" in {
    ArgumentsArity(rest = true) parse Nil should be(
      Right(ArgumentsArity.ParsedArguments(rest = Some(Nil)))
    )

    ArgumentsArity(rest = true) parse scala.List("a", "b") should be(
      Right(ArgumentsArity.ParsedArguments(rest = Some(scala.List("a", "b"))))
    )
  }

  it should "parse rest arguments and required arguments used together" in {
    ArgumentsArity(rest = true, required = 1) parse scala.List("a") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a"),
        rest = Some(Nil),
      ))
    )

    ArgumentsArity(rest = true, required = 1) parse scala.List("a", "b", "c") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a"),
        rest = Some(scala.List("b", "c")),
      ))
    )
  }

  it should "report the right errors in required arguments" in {
    ArgumentsArity(required = 2) parse Nil should be(
      Left(ArgumentsArity.RequiredArgsMissing(got = 0, expected = 2))
    )

    ArgumentsArity(required = 2, rest = true) parse scala.List("a") should be(
      Left(ArgumentsArity.RequiredArgsMissing(got = 1, expected = 2))
    )

    ArgumentsArity() parse scala.List("a", "b") should be(
      Left(ArgumentsArity.TooManyArgs(extra = 2))
    )
  }
}
