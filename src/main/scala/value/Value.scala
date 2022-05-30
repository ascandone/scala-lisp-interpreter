package value


object Value {

  def fromBool[Op](boolean: Boolean): Value[Op] = if (boolean) {
    Symbol("true")
  } else {
    Symbol("false")
  }

  def nil[Op]: Value[Op] = List()
}

sealed trait Value[+Op] {
  def toBool: Boolean = this match {
    case Symbol("false") => false
    case List(scala.Nil) => false
    case _ => true
  }

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
    case CompiledFunction(_, _) => "#<Function>"
    case Closure(_, _) => "#<Closure>"
  }
}

case class Number[+Op](value: Float) extends Value[Op]

case class String[+Op](value: java.lang.String) extends Value[Op]

case class Symbol[+Op](value: java.lang.String) extends Value[Op]

case class List[+Op](value: scala.List[Value[Op]] = scala.List.empty) extends Value[Op]

object List {
  def of[Op](values: Value[Op]*): List[Op] = List(values.toList)
}

case class CompiledFunction[Op](
                                 instructions: Array[Op],
                                 arity: ArgumentsArity = ArgumentsArity(),
                               ) extends Value[Op]

case class Closure[Op](
                        freeVariables: Array[Value[Op]],
                        fn: CompiledFunction[Op],
                      ) extends Value[Op]

case class ArgumentsArity(
                           required: Int = 0,
                           rest: Boolean = false,
                           optionals: Int = 0,
                         ) {

  def size: Int = required + optionals + (if (rest) 1 else 0)

  def parse[Value](args: scala.List[Value]): Either[ArgumentsArity.ParsingReason, ArgumentsArity.ParsedArguments[Value]] = {
    parseRequiredArgs(args).flatMap(res => {
      val (requiredArgs, afterRequired) = res

      // TODO optionals
      val (optionals, afterOptionals) = ((Nil, 0), afterRequired)

      parseRest(afterOptionals).map(rest => {
        ArgumentsArity.ParsedArguments(
          required = requiredArgs,
          optionals = optionals,
          rest = rest,
        )
      })
    })
  }


  private def parseRequiredArgs[Value](args: scala.List[Value]):
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

  private def parseRest[Value](afterOptionals: scala.List[Value]) =
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