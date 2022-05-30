import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._
import value.parser.Parser

class ParserSpec extends AnyFlatSpec with should.Matchers {

  it should "parse symbols" in {
    Parser.run("abc").get should be(
      scala.List(Symbol("abc")))
  }

  it should "parse int numbers" in {
    Parser.run("23").get should be(
      scala.List(Number(23)))
  }

  it should "parse string literals" in {
    Parser.run("\"abc\"").get should be(
      scala.List(String("abc")))
  }

  it should "parse the empty list" in {
    Parser.run("()").get should be(
      scala.List(List()))
  }

  it should "parse a nonempty list" in {
    Parser.run("(1 2 3)").get should be(
      scala.List(List[Nothing](scala.List(1, 2, 3))))
  }

  it should "parse a nested lists" in {
    Parser.run("(1 () (1 2) 3)").get should be(
      scala.List[Value[Nothing]](
        List.of(
          1,
          List.of(),
          List.of(1, 2),
          3)
      )
    )
  }

  it should "parse multiple expressions" in {
    Parser.run("a b c").get should be(
      scala.List(Symbol("a"), Symbol("b"), Symbol("c")))
  }
}