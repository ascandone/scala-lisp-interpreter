package vm

import value._

object Add extends Op2Impl {
  override def apply(x: Value[OpCode], y: Value[OpCode]): Value[OpCode] = (x, y) match {
    case (Number(na), Number(nb)) => Number(na + nb)
    case _ => throw new Exception(s"Add error (expected numbers, got ${x.show} and ${y.show}")
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

object Sleep extends Op1Impl {
  override def apply(ms: Value[OpCode]): Value[OpCode] = ms match {
    case Number(nms) => {
      Thread.sleep(nms.toLong)
      Value.nil
    }
    case _ => throw new Exception("Invalid sleep args")
  }
}