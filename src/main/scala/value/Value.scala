package value

import scala.language.implicitConversions


object Value {

  implicit def fromBool[Op](boolean: Boolean): Value[Op] = if (boolean) {
    Symbol("true")
  } else {
    Symbol("false")
  }

  implicit def toBool[Op](value: Value[Op]): Boolean = value match {
    case Symbol("false") => false
    case List(scala.Nil) => false
    case _ => true
  }

  implicit def fromNumber[Op](n: Float): Value[Op] = Number(n)

  implicit def fromString[Op](s: java.lang.String): Value[Op] = String(s)

  implicit def fromList[Op](l: scala.List[Value[Op]]): Value[Op] = List(l)

  def nil[Op]: Value[Op] = List()
}

sealed trait Value[+Op] {
  def show: java.lang.String = this match {
    case Symbol(name) => name
    case String(str) => s"\"$str\""
    case Number(n) =>
      if (n.toInt == n) {
        n.toInt.toString
      } else {
        n.toString
      }
    case List(scala.Nil) => "nil"
    case List(values) => "(" + values.map(_.show).mkString(" ") + ")"
    case Function(_, _) => "#<Function>"
    case Closure(_, _) => "#<Closure>"
  }
}

case class Number[+Op](value: Float) extends Value[Op]

case class String[+Op](value: java.lang.String) extends Value[Op]

case class Symbol[+Op](value: java.lang.String) extends Value[Op]

case class List[+Op](value: scala.List[Value[Op]] = Nil) extends Value[Op]

object List {
  def of[Op](values: Value[Op]*): List[Op] = List(values.toList)
}

case class Function[Op](
                         instructions: Array[Op],
                         arity: ArgumentsArity = ArgumentsArity(),
                       ) extends Value[Op]

case class Closure[Op](
                        freeVariables: Array[Value[Op]],
                        fn: Function[Op],
                      ) extends Value[Op]

case class ArgumentsArity(
                           required: Int = 0,
                           rest: Boolean = false,
                           optionals: Int = 0,
                         ) {

  def size: Int = required + optionals + (if (rest) 1 else 0)

  def parse[Value](args: scala.List[Value]): Either[ArgumentsArity.ParsingReason, ArgumentsArity.ParsedArguments[Value]] = {
    parseRequired(args).flatMap(res => {
      val (required, afterRequired) = res
      val (optionals, afterOptionals) = parseOpt(afterRequired)

      parseRest(afterOptionals).map(rest => {
        ArgumentsArity.ParsedArguments(
          required = required,
          optionals = optionals,
          rest = rest,
        )
      })
    })
  }

  private def parseRequired[Value](args: scala.List[Value]):
  Either[ArgumentsArity.ParsingReason, (scala.List[Value], scala.List[Value])] =
  // TODO do in one pass
    if (required > args.size) {
      Left(ArgumentsArity.RequiredArgsMissing(
        expected = required,
        got = args.size
      ))
    } else {
      Right(args.splitAt(required))
    }

  private def parseOpt[Value](afterRequired: scala.List[Value]): ((scala.List[Value], Int), scala.List[Value]) = {
    // TODO do in one pass
    val (optionalsArgs, extra) = afterRequired.splitAt(optionals)
    ((optionalsArgs, optionals - optionalsArgs.length), extra)
  }

  private def parseRest[Value](afterOptionals: scala.List[Value]):
  Either[ArgumentsArity.TooManyArgs, Option[scala.List[Value]]] =
    if (rest) {
      // There cannot be `TooManyArgs` with rest args
      Right(Some(afterOptionals))
    } else {
      afterOptionals match {
        case Nil => Right(None)
        case _ => Left(ArgumentsArity.TooManyArgs(extra = afterOptionals.length))
      }
    }
}

object ArgumentsArity {
  sealed trait ParsingReason

  case class ParsedArguments[Value](
                                     required: scala.List[Value] = scala.List(),
                                     optionals: (scala.List[Value], Int) = (scala.List(), 0),
                                     rest: Option[scala.List[Value]] = None,
                                   )

  case class RequiredArgsMissing(expected: Int, got: Int) extends ParsingReason

  case class TooManyArgs(extra: Int) extends ParsingReason
}