package vm

import value._

object Lib {
  def add(a: Value, b: Value): Value = (a, b) match {
    case (Number(na), Number(nb)) => Number(na + nb)
    case _ => throw new Exception("Add error")
  }
}
