package vm

import value._

sealed trait OpCode

case class Push(value: Value[OpCode]) extends OpCode

case object Pop extends OpCode

case class Builtin(op: Op) extends OpCode

case class Jump(target: Int) extends OpCode

case class JumpIfNot(target: Int) extends OpCode

case class SetGlobal(name: Int) extends OpCode

case class GetGlobal(name: Int) extends OpCode

case class SetLocal(ident: Int) extends OpCode

case class GetLocal(ident: Int) extends OpCode

case class Call(passedArgs: Int) extends OpCode

case object Return extends OpCode

case class PushClosure(freeVariables: Int, fn: CompiledFunction[OpCode]) extends OpCode

case class GetFree(ident: Int) extends OpCode

// Builtin
sealed trait Op

object Add extends Op

object GreaterThan extends Op

object IsEq extends Op

object Not extends Op

object Cons extends Op

object First extends Op

object Rest extends Op

object IsNil extends Op

object IsList extends Op

object Log extends Op

object Sleep extends Op

