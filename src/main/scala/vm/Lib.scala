package vm

import value._

object Lib {
  def add[Op](a: Value[Op], b: Value[Op]): Value[Op] = (a, b) match {
    case (Number(na), Number(nb)) => Number(na + nb)
    case _ => throw new Exception("Add error")
  }
}
