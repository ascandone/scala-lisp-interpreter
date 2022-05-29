package value


object Value {

  def fromBool[Op](boolean: Boolean): Value[Op] = if (boolean) {
    Symbol("true")
  } else {
    Symbol("false")
  }
  
  def nil[Op]: Value[Op] = List()
}

sealed trait Value[Op] {
  def toBool: Boolean = this match {
    case Symbol("false") => false
    case List(scala.Nil) => false
    case _ => true
  }
}

case class Number[Op](value: Float) extends Value[Op]

case class String[Op](value: java.lang.String) extends Value[Op]

case class Symbol[Op](value: java.lang.String) extends Value[Op]

case class List[Op](value: scala.List[Value[Op]] = scala.List.empty) extends Value[Op]

case class CompiledFunction[Op](
                                 instructions: Array[Op],
                                 argsNumber: Int = 0,
                                 localsNumber: Int = 0,
                               ) extends Value[Op]

case class Closure[Op](
                        freeVariables: Array[Value[Op]],
                        fn: CompiledFunction[Op],
                      ) extends Value[Op]
