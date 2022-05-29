package vm

import collection.mutable.ArrayStack
import value.Value

import scala.collection.mutable

object Vm {
  def run(instructions: Array[OpCode]): Value = {
    val vm = new Vm(instructions)
    vm.run()
  }
}

private class Frame(val instructions: Array[OpCode]) {
  var ip = 0
}

private class Vm(private var instructions: Array[OpCode]) {
  // TODO this should be outside
  // TODO this should be an array
  private val globals = mutable.HashMap[String, Value]()
  private val stack = new ArrayStack[Value]()
  private val frames = {
    val stack = new ArrayStack[Frame]()
    stack.push(new Frame(instructions))
    stack
  }

  def run(): Value = {
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

    case Op2(f) => {
      val right = stack.pop()
      val left = stack.pop()
      val result = f(left, right)
      stack.push(result)
    }

    case Jump(target) => {
      frames.peek().ip = target
    }

    case JumpIfNot(target) => {
      val value = stack.pop()
      if (!value.toBool) {
        frames.peek().ip = target
      }
    }

    case SetGlobal(name) => {
      val value = stack.pop()
      globals.put(name, value)
      stack.push(Value.nil)
    }

    case GetGlobal(name) => {
      val value = globals(name)
      stack.push(value)
    }
  }
}
