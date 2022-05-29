package vm

import collection.mutable.ArrayStack
import value._

import scala.collection.mutable

object Vm {
  def run(instructions: Array[OpCode]): Value[OpCode] = {
    val vm = new Vm(instructions)
    vm.run()
  }
}

private class Frame(val instructions: Array[OpCode], val basePointer: Int) {
  var ip = 0
}

private class Vm(private var instructions: Array[OpCode]) {
  // TODO this should be outside
  // TODO this should be an array
  private val globals = mutable.HashMap[java.lang.String, Value[OpCode]]()
  private val stack = new ArrayStack[Value[OpCode]]()
  private val frames = {
    val stack = new ArrayStack[Frame]()
    stack.push(new Frame(instructions = instructions, basePointer = 0))
    stack
  }

  def run(): Value[OpCode] = {
    while ( {
      val currentFrame = frames.peek()
      currentFrame.ip < currentFrame.instructions.length
    }) {

      val opCode = {
        val currentFrame = frames.peek()
        val opCode = frames.peek().instructions(currentFrame.ip)
        currentFrame.ip += 1
        opCode
      }

      step(opCode)
    }

    stack.peek()
  }

  private def step(opCode: OpCode): Unit = opCode match {
    case Push(value) => stack.push(value)

    case Pop => stack.pop()

    case Op2(f) =>
      val right = stack.pop()
      val left = stack.pop()
      val result = f(left, right)
      stack.push(result)

    case Jump(target) =>
      frames.peek().ip = target

    case JumpIfNot(target) =>
      val value = stack.pop()
      if (!value.toBool) {
        frames.peek().ip = target
      }

    case SetGlobal(name) =>
      val value = stack.pop()
      globals.put(name, value)
      stack.push(Value.nil)

    case GetGlobal(name) =>
      val value = globals(name)
      stack.push(value)

    case Call(passedArgs) =>
      val value = stack.pop()
      val fn: CompiledFunction[OpCode] = value match {
        case c@CompiledFunction(_, _, _) => c
        case _ => throw new Exception("Expected a function")
      }

      if (fn.argsNumber != passedArgs) {
        throw new Exception("Arity error")
      }

      val newFrame = new Frame(
        instructions = fn.instructions,
        basePointer = stack.length() - passedArgs
      )

      frames.push(newFrame)

    case Return =>
      val retValue = stack.pop()
      val numLocals = stack.length() - frames.peek().basePointer
      for (_ <- 0 until numLocals) {
        stack.pop()
      }
      stack.push(retValue)

    case GetLocal(ident) =>
      val index = frames.peek().basePointer + ident
      val retValue = stack.get(index)
      stack.push(retValue)

    case SetLocal(ident) =>
      val value = stack.peek()
      val index = frames.peek().basePointer + ident
      stack.set(index, value)
  }
}
