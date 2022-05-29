import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._
import vm._

class VmSpec extends AnyFlatSpec with should.Matchers {

  it should "push numbers" in {
    val instructions: Array[OpCode] = Array(Push(Number(42)))
    Vm.run(instructions) should be(Number(42))
  }

  it should "pop values" in {
    val instructions: Array[OpCode] = Array(
      Push(Number(10)),
      Push(Number(20)),
      Pop
    )
    Vm.run(instructions) should be(Number(10))
  }

  it should "execute Op.Op2" in {

    val instructions: Array[OpCode] = Array(
      Push(Number(10)),
      Push(Number(20)),
      Op2(Add),
    )
    Vm.run(instructions) should be(Number(30))
  }


  it should "execute Op.Jump" in {

    val instructions: Array[OpCode] = Array(
      /* 0 */ Push(Number(0)),
      /* 1 */ Jump(3),
      /* 2 */ Push(Number(1)),
      /* 3 */
    )

    Vm.run(instructions) should be(Number(0))
  }


  it should "execute Op.JumpIfNot (false)" in {

    val instructions: Array[OpCode] = Array(
      /* 0 */ Push(Number(0)),
      /* 1 */ Push(Value.fromBool(false)),
      /* 2 */ JumpIfNot(4),
      /* 3 */ Push(Number(1)),
      /* 4 */
    )

    Vm.run(instructions) should be(Number(0))
  }

  it should "execute Op.JumpIfNot (true)" in {

    val instructions: Array[OpCode] = Array(
      /* 0 */ Push(Number(0)),
      /* 1 */ Push(Value.fromBool(true)),
      /* 2 */ JumpIfNot(4),
      /* 3 */ Push(Number(1)),
      /* 4 */
    )

    Vm.run(instructions) should be(Number(1))
  }

  it should "execute Op.GetGlobal/SetGlobal" in {

    val instructions: Array[OpCode] = Array(
      Push(Number(42)),
      SetGlobal(0),
      Pop,
      GetGlobal(0)
    )

    Vm.run(instructions) should be(Number(42))
  }

  it should "execute Op.Call with no args" in {

    val fn = CompiledFunction[OpCode](instructions = Array(
      Push(Number(100)),
      Push(Number(42)),
      Op2(Add),
      Return,
    ))

    val instructions: Array[OpCode] = Array(
      Push(fn),
      Call(0),
    )

    Vm.run(instructions) should be(Number(142))
  }

  it should "execute Op.Call with two args" in {

    val fn = CompiledFunction[OpCode](argsNumber = 2, instructions = Array(
      Push(Value.nil),
      GetLocal(0),
      GetLocal(1),
      Op2(Add),
      Return,
    ))

    val instructions = Array[OpCode](
      Push(Number(100)),
      Push(Number(200)),
      Push(fn),
      Call(2),
    )

    Vm.run(instructions) should be(Number(300))
  }

  it should "execute PushClosure" in {
    // a => b => a + b
    val inner = CompiledFunction[OpCode](
      argsNumber = 1,
      instructions = Array(
        GetFree(0),
        GetLocal(0),
        Op2(Add),
        Return,
      )
    )

    val outer = CompiledFunction[OpCode](
      argsNumber = 1,
      instructions = Array(
        GetLocal(0),
        PushClosure(1, inner),
        Return
      )
    )

    val instructions = Array[OpCode](
      Push(Number(100)),
      Push(Number(200)),
      Push(outer),
      Call(1),
      Call(1),
    )

    Vm.run(instructions) should be(Number(300))
  }

  it should "should execute recursive function" in {
    /*
      (defun f (n)
        (if (> n $LIM)
          n
          (f (+ 1 n))))

      (f 0)
    */

    val LIM = 4

    val fn = CompiledFunction[OpCode](
      argsNumber = 1,
      instructions = Array(
        /* 00 */ GetLocal(0),
        /* 01 */ Push(Number(LIM)),
        /* 02 */ Op2(GreaterThan),
        /* 03 */ JumpIfNot(6),
        /* 04 */ GetLocal(0),
        /* 05 */ Jump(11),
        /* 06 */ Push(Number(1)), // <- 03
        /* 07 */ GetLocal(0),
        /* 08 */ Op2(Add),
        /* 09 */ GetGlobal(0),
        /* 10 */ Call(1),
        /* 11 */ Return,
      )
    )

    val instructions = Array[OpCode](
      Push(fn),
      SetGlobal(0),
      Pop,
      Push(Number(0)),
      GetGlobal(0),
      Call(1),
    )

    Vm.run(instructions) should be(Number(LIM + 1))
  }


}