package vm

import _root_.mutable.ArrayStack
import value.ArgumentsArity.ParsedArguments
import value._
import vm.opcode._

import java.util.concurrent.LinkedTransferQueue
import scala.annotation.tailrec
import scala.collection.mutable

object Vm {
  private def valueToClosure(value: Value[OpCode]): Closure[OpCode] = value match {
    case fn@CompiledFunction(_, _) => Closure(
      freeVariables = Array(),
      fn = fn,
    )

    case closure@Closure(_, _) => closure

    case _ => throw new Exception(s"Expected a function (got ${value.show} instead)")
  }
}

class Vm {
  private val globals = mutable.HashMap[Int, Value[OpCode]]()

  private val queues = {
    val map = mutable.HashMap[Float, LinkedTransferQueue[Value[OpCode]]]()
    map.put(Thread.currentThread().getId.toFloat, new LinkedTransferQueue[Value[OpCode]]())
    map
  }

  def run(instructions: Array[OpCode]): Value[OpCode] = {
    val loop = new VmLoop(instructions)
    loop.run()
  }

  private class VmLoop(private val instructions: Array[OpCode]) {
    private val stack = new ArrayStack[Value[OpCode]]()
    private val frames = ArrayStack(
      new Frame(
        closure = Closure(
          freeVariables = Array(),
          fn = CompiledFunction(instructions)
        ),
        basePointer = 0,
      )
    )

    @tailrec
    final def run(): Value[OpCode] = {
      val currentFrame = frames.peek()
      if (
        currentFrame.ip < currentFrame.instructions.length
      ) {
        val opCode = currentFrame.instructions(currentFrame.ip)
        currentFrame.ip += 1
        execute(opCode)
        run()
      } else {
        stack.peek()
      }
    }

    private def execOp0(f: () => Value[OpCode]): Unit = {
      val result = f()
      stack.push(result)
    }

    private def execOp1(f: (Value[OpCode]) => Value[OpCode]): Unit = {
      val x = stack.pop()
      val result = f(x)
      stack.push(result)
    }

    private def execOp2(f: (Value[OpCode], Value[OpCode]) => Value[OpCode]): Unit = {
      val y = stack.pop()
      val x = stack.pop()
      val result = f(x, y)
      stack.push(result)
    }

    private def execute(opCode: OpCode): Unit = opCode match {
      case Push(value) => stack.push(value)

      case Pop => stack.pop()

      case Jump(target) =>
        frames.peek().ip = target

      case JumpIfNot(target) =>
        val value = stack.pop()
        if (!value) {
          frames.peek().ip = target
        }

      case SetGlobal(ident) =>
        val value = stack.pop()
        globals.put(ident, value)
        stack.push(Nil)

      case GetGlobal(ident) => globals.get(ident) match {
        // Receiving `None` means that (def) was called inside a lambda not yet called
        case None => stack.push(Nil)
        case Some(value) => stack.push(value)
      }

      case Apply => {
        val givenArgs = stack.pop() match {
          case List(lst) => lst
          // TODO better err
          case _ => throw new Exception("Expected a list")
        }

        val closure = Vm.valueToClosure(stack.pop())

        callFunction(closure, givenArgs)
      }

      case Add => execOp2((x, y) => (x, y) match {
        case (Number(na), Number(nb)) => na + nb
        case _ => throw new Exception(s"Add error (expected numbers, got ${x.show} and ${y.show}")
      })


      case GreaterThan => execOp2((a, b) => (a, b) match {
        case (Number(na), Number(nb)) => na > nb
        case _ => throw new Exception("GT error")
      })

      case IsEq => execOp2((x, y) => x == y)

      case Not => execOp1(a => !a)

      case Cons => execOp2((head, tail) => tail match {
        case List(tail_) => List(head :: tail_)
        case _ => throw new Exception("Cons tail should be a list")
      })

      case First => execOp1 {
        case List(Nil) => Nil
        case List(hd :: _) => hd
        case _ => throw new Exception("`first` argument should be a list")
      }

      case Rest => execOp1({
        case List(Nil) => Nil
        case List(_ :: tl) => tl
        case _ => throw new Exception("`rest` argument should be a list")
      })

      case IsNil => execOp1({
        case List(Nil) => true
        case _ => false
      })

      case IsList => execOp1({
        case List(_) => true
        case _ => false
      })

      case Sleep => execOp1({
        case Number(nms) => {
          Thread.sleep(nms.toLong)
          Value.nil
        }
        case _ => throw new Exception("Invalid sleep args")
      })

      case Log => execOp1(x => {
        println(x.show)
        Value.nil
      })

      case Panic => execOp1({
        case String(reason) => throw new Exception(reason)
        case _ => throw new Exception("Invalid panic args (expected a string)")
      })


      case Self => execOp0(() => Thread.currentThread().getId.toFloat)

      case Fork =>
        val closure = Vm.valueToClosure(stack.pop())
        val thread = new Thread {
          override def run(): Unit = {
            val vm = new VmLoop(Array(
              Push(closure),
              Call(0),
            ))
            vm.run()
          }
        }

        thread.setDaemon(true)
        thread.start()
        val id = thread.getId.toFloat
        queues.put(id, new LinkedTransferQueue())
        stack.push(id)

      case Receive =>
        val selfId = Thread.currentThread().getId.toFloat
        val maybeQueue = queues.get(selfId)
        maybeQueue match {
          case None => throw new Exception(s"thread $selfId not found (in receive)")
          case Some(queue) =>
            val value = queue.take()
            stack.push(value)
        }

      case Send =>
        val valueToSend = stack.pop()
        val id = stack.pop() match {
          case Number(n) => n
          case _ => throw new Exception("expected a number")
        }

        val maybeQueue = queues.get(id)
        maybeQueue match {
          case None => throw new Exception(s"thread $id not found (in send)")
          case Some(queue) =>
            queue.transfer(valueToSend)
            stack.push(Nil)
        }

      case Call(argsGivenNumber) =>
        val value = stack.pop()
        val closure = Vm.valueToClosure(value)
        val givenArgs = (0 until argsGivenNumber).map(_ => stack.pop()).reverse.toList
        callFunction(closure, givenArgs)

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

    private def callFunction(closure: Closure[OpCode], givenArgs: scala.List[Value[OpCode]]): Unit =
      closure.fn.arity parse givenArgs match {
        // TODO better error
        case Left(ArgumentsArity.RequiredArgsMissing(expected, got)) => throw new Exception(s"Arity error (expected at least $expected, got $got)")
        case Left(ArgumentsArity.TooManyArgs(extra)) => throw new Exception(s"Arity error (got $extra more)")
        case Right(parsedArgs) =>

          if (frames.peek().closure.fn == closure.fn && closure.fn.instructions(frames.peek().ip) == Return) {
            stack.withPointer(frames.peek().basePointer, {
              handleCallPush(parsedArgs)
            })
            frames.peek().ip = 0
          } else {
            handleCallPush(parsedArgs)

            frames.push(new Frame(
              closure = closure,
              basePointer = stack.length() - closure.fn.arity.size
            ))
          }
      }

    private def handleCallPush(parsedArgs: ParsedArguments[Value[OpCode]]): Unit = {
      for (arg <- parsedArgs.required) {
        stack.push(arg)
      }

      val (optionalPassed, optionalsNotGiven) = parsedArgs.optionals

      for (arg <- optionalPassed) {
        stack.push(arg)
      }

      for (_ <- 0 until optionalsNotGiven) {
        stack.push(Value.nil)
      }

      for (restArgs <- parsedArgs.rest) {
        stack.push(List(restArgs))
      }
    }
  }
}


private class Frame(val closure: Closure[OpCode], val basePointer: Int) {
  var ip = 0

  def instructions: Array[OpCode] = closure.fn.instructions
}
