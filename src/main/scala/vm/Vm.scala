package vm

import value.ArgumentsArity.ParsedArguments
import value._
import vm.mutable.ArrayStack

import scala.collection.mutable

object Vm {
  def runOnce(instructions: Array[OpCode]): Value[OpCode] = {
    val vm = new Vm
    vm.run(instructions)
  }
}

class Vm {
  private val globals = mutable.HashMap[Int, Value[OpCode]]()

  def run(instructions: Array[OpCode]): Value[OpCode] = {
    val loop = new VmLoop(instructions)
    loop.run()
  }

  private class VmLoop(private var instructions: Array[OpCode]) {
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

      case Builtin(op) => op match {
        case Add => op2((x, y) =>
          (x, y) match {
            case (Number(na), Number(nb)) => Number(na + nb)
            case _ => throw new Exception(s"Add error (expected numbers, got ${x.show} and ${y.show}")
          })

        case IsEq => op2((x, y) => x == y)

        case GreaterThan => op2((a, b) => (a, b) match {
          case (Number(na), Number(nb)) => na > nb
          case _ => throw new Exception("GT error")
        })

        case Not => op1(a => !a.toBool)

        case Cons => op2((head, tail) =>
          tail match {
            case List(tail_) => List(head :: tail_)
            case _ => throw new Exception("Cons tail should be a list")
          })

        case First => op1 {
          case List(Nil) => Nil
          case List(hd :: _) => hd
          case _ => throw new Exception("`first` argument should be a list")
        }

        case Rest => op1 {
          case List(Nil) => Nil
          case List(_ :: tl) => tl
          case _ => throw new Exception("`first` argument should be a list")
        }

        case IsNil => op1 {
          case List(Nil) => true
          case _ => false
        }

        case IsList => op1 {
          case List(_) => true
          case _ => false
        }

        case Sleep => op1 {
          case Number(nms) => {
            Thread.sleep(nms.toLong)
            Value.nil
          }
          case _ => throw new Exception("Invalid sleep args")
        }

        case Log => op1(x => {
          println(x.show)
          Value.nil
        })

      }

      case Jump(target) =>
        frames.peek().ip = target

      case JumpIfNot(target) =>
        val value = stack.pop()
        if (!value.toBool) {
          frames.peek().ip = target
        }

      case SetGlobal(ident) =>
        val value = stack.pop()
        globals.put(ident, value)
        stack.push(Value.nil)

      case GetGlobal(ident) => globals.get(ident) match {
        // Receiving `None` means that (def) was called inside a lambda not yet called
        case None => stack.push(List.of())
        case Some(value) => stack.push(value)
      }

      case Call(argsGivenNumber) =>
        val value = stack.pop()
        val closure: Closure[OpCode] = value match {
          case fn@CompiledFunction(_, _) => Closure(
            freeVariables = Array(),
            fn = fn,
          )

          case closure@Closure(_, _) => closure

          case _ => throw new Exception("Expected a function")
        }

        val givenArgs = (0 until argsGivenNumber).map(_ => stack.pop()).reverse.toList

        closure.fn.arity parse givenArgs match {
          // TODO better error
          case Left(ArgumentsArity.RequiredArgsMissing(expected, got)) => throw new Exception(s"Arity error (expected at least $expected, got $got)")
          case Left(ArgumentsArity.TooManyArgs(extra)) => throw new Exception(s"Arity error (got $extra more)")
          case Right(parsedArgs) => {

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
        }

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

    private def op2(f: (Value[OpCode], Value[OpCode]) => Value[OpCode]): Unit = {
      val y = stack.pop()
      val x = stack.pop()
      val ret = f(x, y)
      stack.push(ret)
    }

    private def op1(f: Value[OpCode] => Value[OpCode]): Unit = {
      val x = stack.pop()
      val ret = f(x)
      stack.push(ret)
    }
  }
}


private class Frame(val closure: Closure[OpCode], val basePointer: Int) {
  var ip = 0
}
