import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._
import vm._

class VmSpec extends AnyFlatSpec with should.Matchers {

  it should "push numbers" in {
    val instructions: Array[OpCode] = Array(Push(42))
    Vm.runOnce(instructions) should be(Number(42))
  }

  it should "pop values" in {
    val instructions: Array[OpCode] = Array(
      Push(10),
      Push(20),
      Pop
    )
    Vm.runOnce(instructions) should be(Number(10))
  }

  it should "execute Op.Op2" in {

    val instructions: Array[OpCode] = Array(
      Push(10),
      Push(20),
      Op2(Add),
    )
    Vm.runOnce(instructions) should be(Number(30))
  }


  it should "execute Op.Jump" in {

    val instructions: Array[OpCode] = Array(
      /* 0 */ Push(0),
      /* 1 */ Jump(3),
      /* 2 */ Push(1),
      /* 3 */
    )

    Vm.runOnce(instructions) should be(Number(0))
  }


  it should "execute Op.JumpIfNot (false)" in {

    val instructions: Array[OpCode] = Array(
      /* 0 */ Push(0),
      /* 1 */ Push(false),
      /* 2 */ JumpIfNot(4),
      /* 3 */ Push(1),
      /* 4 */
    )

    Vm.runOnce(instructions) should be(Number(0))
  }

  it should "execute Op.JumpIfNot (true)" in {

    val instructions: Array[OpCode] = Array(
      /* 0 */ Push(0),
      /* 1 */ Push(true),
      /* 2 */ JumpIfNot(4),
      /* 3 */ Push(1),
      /* 4 */
    )

    Vm.runOnce(instructions) should be(Number(1))
  }

  it should "execute Op.GetGlobal/SetGlobal" in {

    val instructions: Array[OpCode] = Array(
      Push(42),
      SetGlobal(0),
      Pop,
      GetGlobal(0)
    )

    Vm.runOnce(instructions) should be(Number(42))
  }

  it should "execute Op.Call with no args" in {

    val fn = CompiledFunction[OpCode](instructions = Array(
      Push(100),
      Push(42),
      Op2(Add),
      Return,
    ))

    val instructions: Array[OpCode] = Array(
      Push(fn),
      Call(0),
    )

    Vm.runOnce(instructions) should be(Number(142))
  }

  it should "execute Op.Call with two args" in {

    val fn = CompiledFunction[OpCode](arity = ArgumentsArity(required = 2), instructions = Array(
      Push(Value.nil),
      GetLocal(0),
      GetLocal(1),
      Op2(Add),
      Return,
    ))

    val instructions = Array[OpCode](
      Push(100),
      Push(200),
      Push(fn),
      Call(2),
    )

    Vm.runOnce(instructions) should be(Number(300))
  }

  it should "execute PushClosure" in {
    // a => b => a + b
    val inner = CompiledFunction[OpCode](
      arity = ArgumentsArity(required = 1),
      instructions = Array(
        GetFree(0),
        GetLocal(0),
        Op2(Add),
        Return,
      )
    )

    val outer = CompiledFunction[OpCode](
      arity = ArgumentsArity(required = 1),
      instructions = Array(
        GetLocal(0),
        PushClosure(1, inner),
        Return
      )
    )

    val instructions = Array[OpCode](
      Push(100),
      Push(200),
      Push(outer),
      Call(1),
      Call(1),
    )

    Vm.runOnce(instructions) should be(Number(300))
  }

  it should "should execute recursive function" in {
    /*
      (defun f (n)
        (if (> n $LIM)
          n
          (f (+ 1 n))))

      (f 0)
    */

    val LIM = 4.toFloat

    val fn = CompiledFunction[OpCode](
      arity = ArgumentsArity(required = 1),
      instructions = Array(
        /* 00 */ GetLocal(0),
        /* 01 */ Push(LIM),
        /* 02 */ Op2(GreaterThan),
        /* 03 */ JumpIfNot(6),
        /* 04 */ GetLocal(0),
        /* 05 */ Jump(11),
        /* 06 */ Push(1), // <- 03
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
      Push(0),
      GetGlobal(0),
      Call(1),
    )

    Vm.runOnce(instructions) should be(Number((LIM + 1).toFloat))
  }

  it should "handle rest params" in {
    // ((lambda (&rest xs) xs) 0 1 2)
    val fn = CompiledFunction[OpCode](
      arity = ArgumentsArity(rest = true),
      instructions = Array(
        GetLocal(0),
        Return,
      ))

    val instructions = Array[OpCode](
      Push(0),
      Push(1),
      Push(2),
      Push(fn),
      Call(3),
    )

    Vm.runOnce(instructions) should be(List.of(
      Number(0),
      Number(1),
      Number(2),
    ))
  }

  it should "handle optional params" in {
    // ((lambda (&opt a b) (cons a b)) 0)
    val fn = CompiledFunction[OpCode](
      arity = ArgumentsArity(optionals = 2),
      instructions = Array(
        GetLocal(0),
        GetLocal(1),
        Op2(Cons),
        Return,
      ))

    val instructions = Array[OpCode](
      Push(0),
      Push(fn),
      Call(1),
    )

    Vm.runOnce(instructions) should be(List.of(
      Number(0),
    ))
  }
}