package value.parser

import value._

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  def run(input: CharSequence): ParseResult[scala.List[Value[Nothing]]] = parseAll(program, input)

  def program: Parser[scala.List[Value[Nothing]]] = rep(value)

  def symbol: Parser[Symbol[Nothing]] =
  // TODO reuse rs
    """[a-zA-Z_+<>\-*!/][a-zA-Z_+<>\-*!/0-9]*""".r ^^ Symbol[Nothing]

  private def expr: Parser[List[Nothing]] = ("(" ~> rep(value) <~ ")") ^^ List[Nothing]

  private def value: Parser[Value[Nothing]] = numberLiteral | stringLiteral | symbol | expr

  private def numberLiteral: Parser[Number[Nothing]] = """[1-9][0-9]*|0""".r ^^ { value => Number(value.toFloat) }

  private def stringLiteral: Parser[String[Nothing]] = "\"" ~> """[a-zA-Z0-9:*/+\- !]*""".r <~ "\"" ^^ String[Nothing]
}
