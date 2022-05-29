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
      Op2(Lib.add),
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
      /* 1 */ Push(Value.false_),
      /* 2 */ JumpIfNot(4),
      /* 3 */ Push(Number(1)),
      /* 4 */
    )

    Vm.run(instructions) should be(Number(0))
  }

  it should "execute Op.JumpIfNot (true)" in {

    val instructions: Array[OpCode] = Array(
      /* 0 */ Push(Number(0)),
      /* 1 */ Push(Value.true_),
      /* 2 */ JumpIfNot(4),
      /* 3 */ Push(Number(1)),
      /* 4 */
    )

    Vm.run(instructions) should be(Number(1))
  }

  it should "execute Op.GetGlobal/SetGlobal" in {

    val instructions: Array[OpCode] = Array(
      Push(Number(42)),
      SetGlobal("x"),
      Pop,
      GetGlobal("x")
    )

    Vm.run(instructions) should be(Number(42))
  }

}