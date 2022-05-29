package vm

import value._

object Add extends Op2Impl {
  override def apply(x: Value[OpCode], y: Value[OpCode]): Value[OpCode] = (x, y) match {
    case (Number(na), Number(nb)) => Number(na + nb)
    case _ => throw new Exception("Add error")
  }
}

object GreaterThan extends Op2Impl {
  override def apply(a: Value[OpCode], b: Value[OpCode]): Value[OpCode] = (a, b) match {
    case (Number(na), Number(nb)) => Value.fromBool(na > nb)
    case _ => throw new Exception("GT error")
  }
}

object Not extends Op1Impl {
  override def apply(a: Value[OpCode]): Value[OpCode] = Value.fromBool(!a.toBool)
}
