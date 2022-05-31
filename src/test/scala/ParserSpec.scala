import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._
import value.parser.Parser

class ParserSpec extends AnyFlatSpec with should.Matchers {

  it should "parse symbols" in {
    Parser.run("abc").get.head should be(Symbol("abc"))
  }

  it should "parse int numbers" in {
    Parser.run("23").get.head should be(Number(23))
  }

  it should "parse string literals" in {
    Parser.run("\"abc\"").get.head should be(String("abc"))
  }

  it should "parse whitespace and comments" in {
    Parser.run(
      """
            a ; comment
            b
        """
    ).get should be(scala.List(
      Symbol("a"), Symbol("b")
    ))
  }

  it should "parse the empty list" in {
    Parser.run("()").get.head should be(List())
  }

  it should "parse a nonempty list" in {
    Parser.run("(1 2 3)").get.head should be(List.of(1, 2, 3))
  }

  it should "parse nested lists" in {
    Parser.run("(1 () (1 2) 3)").get.head should be(
      List.of(1, Nil, List.of(1, 2), 3)
    )
  }

  it should "parse multiple expressions" in {
    Parser.run("a b c").get should be(
      scala.List(Symbol("a"), Symbol("b"), Symbol("c")))
  }

  it should "handle quote sugar" in {
    Parser.run("'x").get.head should be(List.of(
      Symbol("quote"),
      Symbol("x"),
    ))

    Parser.run("'  x").get.head should be(List.of(
      Symbol("quote"),
      Symbol("x"),
    ))

    Parser.run("'  ()").get.head should be(List.of(
      Symbol("quote"),
      Nil,
    ))
  }

  it should "handle backquote sugar" in {
    Parser.run("`x").get.head should be(List.of(
      Symbol("backquote"),
      Symbol("x"),
    ))
  }

  it should "handle unquote sugar" in {
    Parser.run(",x").get.head should be(List.of(
      Symbol("unquote"),
      Symbol("x"),
    ))
  }

  it should "handle unquote splicing sugar" in {
    Parser.run(",@x").get.head should be(List.of(
      Symbol("unquote-splicing"),
      Symbol("x"),
    ))
  }
}