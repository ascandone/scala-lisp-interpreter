package vm.opcode

import value._

sealed trait OpCode

case class Push(value: Value[OpCode]) extends OpCode

case object Pop extends OpCode

case class Jump(target: Int) extends OpCode

case class JumpIfNot(target: Int) extends OpCode

case class SetGlobal(name: Int) extends OpCode

case class GetGlobal(name: Int) extends OpCode

case class SetLocal(ident: Int) extends OpCode

case class GetLocal(ident: Int) extends OpCode

case class Call(passedArgs: Int) extends OpCode

case object Apply extends OpCode

case object Return extends OpCode

case class PushClosure(freeVariables: Int, fn: CompiledFunction[OpCode]) extends OpCode

case class GetFree(ident: Int) extends OpCode

case object Add extends OpCode

case object GreaterThan extends OpCode

case object IsEq extends OpCode

case object Not extends OpCode

case object Cons extends OpCode

case object First extends OpCode

case object Rest extends OpCode

case object IsNil extends OpCode

case object IsList extends OpCode

case object Sleep extends OpCode

case object Panic extends OpCode

case object Log extends OpCode

case object Fork extends OpCode

case object Receive extends OpCode

case object Send extends OpCode

case object Self extends OpCode