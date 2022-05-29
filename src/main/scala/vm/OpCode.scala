package vm

import value._

sealed trait OpCode

case class Push(value: Value[OpCode]) extends OpCode

case object Pop extends OpCode

case class Op1(fn: (Value[OpCode]) => Value[OpCode]) extends OpCode

case class Op2(fn: (Value[OpCode], Value[OpCode]) => Value[OpCode]) extends OpCode

case class Jump(target: Int) extends OpCode

case class JumpIfNot(target: Int) extends OpCode

case class SetGlobal(name: java.lang.String) extends OpCode

case class GetGlobal(name: java.lang.String) extends OpCode

case class SetLocal(ident: Int) extends OpCode

case class GetLocal(ident: Int) extends OpCode

case class Call(passedArgs: Int) extends OpCode

case object Return extends OpCode

case class PushClosure(freeVariables: Int, fn: CompiledFunction[OpCode]) extends OpCode

case class GetFree(ident: Int) extends OpCode
