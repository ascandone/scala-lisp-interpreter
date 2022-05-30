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
                                 args: Arguments = Arguments(),
                               ) extends Value[Op]

case class Arguments(
                      required: Int = 0,
                      rest: Boolean = false,
                      optionals: Int = 0,
                    )

case class Closure[Op](
                        freeVariables: Array[Value[Op]],
                        fn: CompiledFunction[Op],
                      ) extends Value[Op]
