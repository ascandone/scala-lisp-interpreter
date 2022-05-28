package value.parser

import value._

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  def run(input: CharSequence): ParseResult[scala.List[Value]] = parseAll(program, input)

  def program: Parser[scala.List[Value]] = rep(value)

  def symbol: Parser[Symbol] =
    """[A-Za-z_+\-*/][a-zA-Z0-9]*""".r ^^ Symbol

  private def expr: Parser[List] = ("(" ~> rep(value) <~ ")") ^^ List

  private def value: Parser[Value] = numberLiteral | stringLiteral | symbol | expr

  private def numberLiteral: Parser[Number] = """[1-9][0-9]*|0""".r ^^ { value => Number(value.toFloat) }

  private def stringLiteral: Parser[String] = "\"" ~> """[a-zA-Z0-9:*/+\- !]*""".r <~ "\"" ^^ String
}
