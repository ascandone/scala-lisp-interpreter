package value

object Value {
  val true_ : Value = Symbol("true")
  val false_ : Value = Symbol("false")
  val nil: Value = List()
}

sealed trait Value {
  def toBool: Boolean = this match {
    case Symbol("false") => false
    case List(scala.Nil) => false
    case _ => true
  }
}

case class Number(value: Float) extends Value

case class String(value: java.lang.String) extends Value

case class Symbol(value: java.lang.String) extends Value

case class List(value: scala.List[Value] = scala.List.empty) extends Value
