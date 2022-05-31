package value.parser

import value._

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  def run(input: CharSequence): ParseResult[scala.List[Value[Nothing]]] = parseAll(program, input)

  def program: Parser[scala.List[Value[Nothing]]] = rep(value)

  def symbol: Parser[Symbol[Nothing]] =
  // TODO reuse rs
    """[a-zA-Z&_+<>\-*!/][a-zA-Z&_+<>\-*!/0-9]*""".r ^^ Symbol[Nothing]

  private def form: Parser[List[Nothing]] = ("(" ~> rep(value) <~ ")") ^^ List[Nothing]

  private def value: Parser[Value[Nothing]] =
    numberLiteral | stringLiteral | symbol | quotationSugar | form

  private def numberLiteral: Parser[Number[Nothing]] = """[1-9][0-9]*|0""".r ^^ { value => Number(value.toFloat) }

  private def stringLiteral: Parser[String[Nothing]] = "\"" ~> """[a-zA-Z0-9:*/+\- !]*""".r <~ "\"" ^^ String[Nothing]

  private def quotationSugar: Parser[Value[Nothing]] =
    quotedValue("'", "quote") | quotedValue("`", "backquote") | quotedValue(",", "unquote") | quotedValue(",@", "unquote-splicing")

  private def quotedValue(token: java.lang.String, name: java.lang.String): Parser[Value[Nothing]] =
    token ~> value ^^ { value => List.of(Symbol(name), value) }
}
