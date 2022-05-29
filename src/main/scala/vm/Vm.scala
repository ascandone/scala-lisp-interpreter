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

private class Frame(val closure: Closure[OpCode], val basePointer: Int) {
  var ip = 0
}

private class Vm(private var instructions: Array[OpCode]) {
  // TODO this should be outside
  // TODO this should be an array
  private val globals = mutable.HashMap[java.lang.String, Value[OpCode]]()
  private val stack = new ArrayStack[Value[OpCode]]()
  private val frames = {
    val stack = new ArrayStack[Frame]()
    val initialFrame = new Frame(
      closure = Closure(
        freeVariables = Array(),
        fn = CompiledFunction(instructions)
      ),
      basePointer = 0,
    )
    stack.push(initialFrame)
    stack
  }

  def run(): Value[OpCode] = {
    while ( {
      val currentFrame = frames.peek()
      currentFrame.ip < currentFrame.closure.fn.instructions.length
    }) {

      val opCode = {
        val currentFrame = frames.peek()
        val opCode = frames.peek().closure.fn.instructions(currentFrame.ip)
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

    case Op1(f) =>
      val value = stack.pop()
      val result = f(value)
      stack.push(result)

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
      val closure: Closure[OpCode] = value match {
        case fn@CompiledFunction(_, _, _) => Closure(
          freeVariables = Array(),
          fn = fn,
        )

        case closure@Closure(_, _) => closure

        case _ => throw new Exception("Expected a function")
      }

      if (closure.fn.argsNumber != passedArgs) {
        throw new Exception("Arity error")
      }

      val newFrame = new Frame(
        closure = closure,
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
      frames.pop()

    case GetLocal(ident) =>
      val index = frames.peek().basePointer + ident
      val retValue = stack.get(index)
      stack.push(retValue)

    case SetLocal(ident) =>
      // TODO test
      val value = stack.peek()
      val index = frames.peek().basePointer + ident
      stack.set(index, value)

    case PushClosure(freeVariablesNum, fn) =>
      val freeVariables = new Array[Value[OpCode]](freeVariablesNum)
      for (i <- 0 until freeVariablesNum) {
        val value = stack.pop()
        freeVariables(i) = value
      }
      stack.push(Closure(freeVariables, fn))

    case GetFree(ident) =>
      val value = frames.peek().closure.freeVariables(ident)
      stack.push(value)
  }
}