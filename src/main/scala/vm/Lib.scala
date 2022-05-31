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
    case (Number(na), Number(nb)) => na > nb
    case _ => throw new Exception("GT error")
  }
}

object Not extends Op1Impl {
  override def apply(a: Value[OpCode]): Value[OpCode] = !a.toBool
}

object Cons extends Op2Impl {
  override def apply(head: Value[OpCode], tail: Value[OpCode]): Value[OpCode] = tail match {
    case List(tail_) => List(head :: tail_)
    case _ => throw new Exception("Cons tail should be a list")
  }
}

object First extends Op1Impl {
  override def apply(lst: Value[OpCode]): Value[OpCode] = lst match {
    case List(Nil) => Nil
    case List(hd :: _) => hd
    case _ => throw new Exception("`first` argument should be a list")
  }
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