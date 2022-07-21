package vm

import _root_.mutable.ArrayStack
import value.ArgumentsArity.ParsedArguments
import value._
import vm.opcode._

import java.util.concurrent.LinkedTransferQueue
import scala.annotation.tailrec
import scala.collection.mutable


final case class RuntimeError(
                               message: java.lang.String,
                               cause: Throwable = None.orNull
                             ) extends Exception(message, cause)


object Vm {
  private def valueToClosure(value: Value[OpCode]): Closure[OpCode] = value match {
    case fn@Function(_, _, _) => Closure(
      freeVariables = Array(),
      fn = fn,
    )

    case closure@Closure(_, _) => closure

    case _ => throw RuntimeError(s"Expected a function (got ${value.show} instead)")
  }
}

class Vm {
  private val globals = mutable.HashMap[Int, Value[OpCode]]()
  private var genSymCount = 0

  def getGlobal(ident: Int): Option[Value[OpCode]] =
    globals.get(ident)

  private val queues = {
    val map = mutable.HashMap[Double, LinkedTransferQueue[Value[OpCode]]]()
    map.put(Thread.currentThread().getId.toDouble, new LinkedTransferQueue[Value[OpCode]]())
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
          fn = Function(instructions)
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

    private def execOp1(f: Value[OpCode] => Value[OpCode]): Unit = {
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

      case Apply =>
        val givenArgs = stack.pop() match {
          case List(lst) => lst
          // TODO better err
          case _ => throw RuntimeError("Expected a list")
        }
        val closure = Vm.valueToClosure(stack.pop())
        callFunction(closure, givenArgs)

      case Add => execOp2((x, y) => (x, y) match {
        case (Number(na), Number(nb)) => na + nb
        case (String(na), String(nb)) => na + nb
        case _ => throw RuntimeError(s"Add error (expected two numbers or strings, got ${x.show} and ${y.show}")
      })

      case Mult => execOp2((x, y) => (x, y) match {
        case (Number(na), Number(nb)) => na * nb
        case _ => throw RuntimeError(s"Mult error (expected numbers, got ${x.show} and ${y.show}")
      })

      case Sub => execOp2((x, y) => (x, y) match {
        case (Number(na), Number(nb)) => na - nb
        case _ => throw RuntimeError(s"Sub error (expected numbers, got ${x.show} and ${y.show}")
      })

      case GreaterThan => execOp2((a, b) => (a, b) match {
        case (Number(na), Number(nb)) => na > nb
        case _ => throw RuntimeError(s"error in >")
      })

      case LesserThan => execOp2((a, b) => (a, b) match {
        case (Number(na), Number(nb)) => na < nb
        case _ => throw RuntimeError("error in <")
      })

      case IsEq => execOp2((x, y) => x == y)

      case Not => execOp1(a => !a)

      case Cons => execOp2((head, tail) => tail match {
        case List(tail_) => List(head :: tail_)
        case _ => throw RuntimeError("Cons tail should be a list")
      })

      case First => execOp1 {
        case List(Nil) => Nil
        case List(hd :: _) => hd
        case arg => throw RuntimeError(s"`first` argument should be a list (got ${arg.show} instead)")
      }

      case Rest => execOp1({
        case List(Nil) => Nil
        case List(_ :: tl) => tl
        case arg => throw RuntimeError(s"`rest` argument should be a list (got ${arg.show} instead)")
      })

      case IsNil => execOp1({
        case List(Nil) => true
        case _ => false
      })

      case IsList => execOp1({
        case List(_) => true
        case _ => false
      })

      case IsNumber => execOp1({
        case Number(_) => true
        case _ => false
      })

      case IsString => execOp1({
        case String(_) => true
        case _ => false
      })

      case IsSymbol => execOp1({
        case Symbol(_) => true
        case _ => false
      })

      case Str => execOp1({
        case String(s) => s
        case x => x.show
      })

      case Sleep => execOp1({
        case Number(nms) =>
          Thread.sleep(nms.toLong)
          Nil

        case _ => throw RuntimeError("Invalid sleep args")
      })

      case Log => execOp1 {
        case List(args) =>
          println(args.map(_.show).mkString(" "))
          Nil

        case _ => throw RuntimeError("Expected a list in `log`")
      }

      case Panic => execOp1({
        case String(reason) => throw RuntimeError(reason)
        case _ => throw RuntimeError("Invalid panic args (expected a string)")
      })

      case Self => execOp0(() => Thread.currentThread().getId.toDouble)

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
        val id = thread.getId.toDouble
        queues.put(id, new LinkedTransferQueue())
        stack.push(id)

      case Receive =>
        val selfId = Thread.currentThread().getId.toDouble
        val maybeQueue = queues.get(selfId)
        maybeQueue match {
          case None => throw RuntimeError(s"thread $selfId not found (in receive)")
          case Some(queue) =>
            val value = queue.take()
            stack.push(value)
        }

      case Send =>
        val valueToSend = stack.pop()
        val id = stack.pop() match {
          case Number(n) => n
          case _ => throw RuntimeError("expected a number")
        }

        val maybeQueue = queues.get(id)
        maybeQueue match {
          case None => throw RuntimeError(s"thread $id not found (in send)")
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

      case SetLocal(ident) =>
        val value = stack.pop()
        val index = frames.peek().basePointer + ident
        stack.set(index, value)

      case GetLocal(ident) =>
        val index = frames.peek().basePointer + ident
        val retValue = stack.get(index)
        stack.push(retValue)

      case PushClosure(freeVariablesNum, fn) =>
        val freeVariables = (0 until freeVariablesNum).map(_ => stack.pop()).reverse.toArray
        stack.push(Closure(freeVariables, fn))

      case GetFree(ident) =>
        val value = frames.peek().closure.freeVariables(ident)
        stack.push(value)

      case GenSym =>
        val newSym = genSymCount
        genSymCount += 1
        stack.push(Symbol("G__" ++ newSym.toString))

      case MakeSymbol => execOp1({
        case String(s) => Symbol(s)
        case Symbol(s) => Symbol(s)
        case _ => throw RuntimeError("Illegal conversion to symbol")
      })

      case Now => execOp0(() => System.currentTimeMillis())

      case Dis => {
        def printInstr(instructions: Array[OpCode]): Unit = {
          val padding = instructions.length.toString.length
          println(instructions.zipWithIndex.map((x) => s"${x._2.toString.reverse.padTo(padding, '0').reverse} ${x._1}").mkString("\n"))
        }

        execOp1({
          case Function(instructions, _, _) => {
            printInstr(instructions)
            Nil
          }
          case Closure(_, fn) => {
            printInstr(fn.instructions)
            Nil
          }
          case _ => throw RuntimeError("Invalid dis arg")
        })
      }
    }

    @tailrec
    private def isTailCall(closure: Closure[OpCode], ip: Int): Boolean =
      closure.fn.instructions(ip) match {
        case Return => true
        case Jump(target) => isTailCall(closure, target)
        case _ => false
      }

    private def callFunction(closure: Closure[OpCode], givenArgs: scala.List[Value[OpCode]]): Unit =
      closure.fn.arity parse givenArgs match {
        // TODO better error
        case Left(ArgumentsArity.RequiredArgsMissing(expected, got)) => throw RuntimeError(s"Arity error (expected at least $expected, got $got)")
        case Left(ArgumentsArity.TooManyArgs(extra)) => throw RuntimeError(s"Arity error (got $extra more)")
        case Right(parsedArgs) =>

          if (
            frames.peek().closure.fn == closure.fn && isTailCall(closure, frames.peek().ip)
          ) {
            stack.withPointer(frames.peek().basePointer, {
              handleCallPush(parsedArgs)
            })
            frames.peek().ip = 0
          } else {
            val basePointer = stack.length()
            handleCallPush(parsedArgs)

            frames.push(new Frame(
              closure = closure,
              basePointer = basePointer
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
        stack.push(Nil)
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
