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
  it should "parse (ƒ) forms" in {
    ArgumentsArity() parse scala.List() should be(
      Right(ArgumentsArity.ParsedArguments())
    )
  }

  it should "parse (ƒ a .. b) forms" in {
    ArgumentsArity(required = 2) parse scala.List("a", "b") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a", "b"),
      ))
    )
  }

  it should "parse (ƒ &rest a .. b) forms" in {
    val arity = ArgumentsArity(rest = true)
    arity parse Nil should be(
      Right(ArgumentsArity.ParsedArguments(rest = Some(Nil)))
    )

    arity parse scala.List("a", "b") should be(
      Right(ArgumentsArity.ParsedArguments(rest = Some(scala.List("a", "b"))))
    )
  }

  it should "parse (ƒ a .. b &rest c .. d) forms" in {
    val arity = ArgumentsArity(rest = true, required = 1)
    arity parse scala.List("a") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a"),
        rest = Some(Nil),
      ))
    )

    arity parse scala.List("a", "b", "c") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a"),
        rest = Some(scala.List("b", "c")),
      ))
    )
  }

  it should "parse (ƒ &opt a .. b) forms" in {
    val arity = ArgumentsArity(optionals = 2)

    arity parse scala.List() should be(
      Right(ArgumentsArity.ParsedArguments(
        optionals = (Nil, 2)
      ))
    )

    arity parse scala.List("a") should be(
      Right(ArgumentsArity.ParsedArguments(
        optionals = (scala.List("a"), 1)
      ))
    )

    arity parse scala.List("a", "b") should be(
      Right(ArgumentsArity.ParsedArguments(
        optionals = (scala.List("a", "b"), 0)
      ))
    )
  }

  it should "parse (ƒ a .. b &opt c .. d)" in {
    val arity = ArgumentsArity(required = 1, optionals = 1)

    arity parse scala.List("a") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a"),
        optionals = (Nil, 0)
      ))
    )

    arity parse scala.List("a", "b") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a"),
        optionals = (Nil, 0)
      ))
    )
  }

  it should "parse (ƒ a .. b &opt c .. d &rest e .. f)" in {
    val arity = ArgumentsArity(required = 1, optionals = 1, rest = true)

    arity parse scala.List("a") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a"),
        optionals = (Nil, 1),
        rest = Some(Nil)
      ))
    )

    arity parse scala.List("a", "b") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a"),
        optionals = (scala.List("a"), 0),
        rest = Some(Nil)
      ))
    )

    arity parse scala.List("a", "b", "c") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a"),
        optionals = (scala.List("a"), 0),
        rest = Some(scala.List("c"))
      ))
    )

    arity parse scala.List("a", "b", "c", "d") should be(
      Right(ArgumentsArity.ParsedArguments(
        required = scala.List("a"),
        optionals = (scala.List("a"), 0),
        rest = Some(scala.List("c", "d"))
      ))
    )
  }

  it should "report when arguments are missing" in {
    ArgumentsArity(required = 2) parse Nil should be(
      Left(ArgumentsArity.RequiredArgsMissing(got = 0, expected = 2))
    )

    ArgumentsArity(required = 2, rest = true) parse scala.List("a") should be(
      Left(ArgumentsArity.RequiredArgsMissing(got = 1, expected = 2))
    )
  }

  it should "report when there are too many args" in {
    ArgumentsArity() parse scala.List("a", "b") should be(
      Left(ArgumentsArity.TooManyArgs(extra = 2))
    )
  }
}
