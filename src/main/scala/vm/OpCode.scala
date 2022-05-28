package vm

import value.Value

sealed trait OpCode

case class Push(value: Value) extends OpCode

case object Pop extends OpCode

case class Op2(fn: (Value, Value) => Value) extends OpCode

case class Jump(target: Int) extends OpCode

case class JumpIfNot(target: Int) extends OpCode

/*
type 'v opcode =
  | Jump of int
  | JumpIfNot of int
  | Op1 of ('v -> 'v)
  | SetGlobal of string
  | GetGlobal of string
  | SetLocal of int
  | GetLocal of int
  | GetFree of int
  | PushClosure of (int * 'v)
  | Call of int
  | Return

 */