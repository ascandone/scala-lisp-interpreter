import org.scalatest.flatspec._
import org.scalatest.matchers._
import vm.mutable.ArrayStack

class ArrayStackSpec extends AnyFlatSpec with should.Matchers {

  it should "execute push and pop" in {
    val stack = new ArrayStack[String]()

    stack.push("first")
    stack.push("second")
    stack.length() should be(2)

    stack.pop() should be("second")

    stack.push("third")
    stack.pop() should be("third")
    stack.pop() should be("first")
  }

  it should "execute get" in {
    val stack = new ArrayStack[String]()

    stack.push("first")
    stack.push("second")
    stack.push("third")

    stack.get(0) should be("first")
    stack.get(2) should be("third")

  }

  it should "execute set" in {
    val stack = new ArrayStack[String]()

    stack.push("first")
    stack.push("second")
    stack.push("third")


    stack.set(0, "first-set")
    stack.get(0) should be("first-set")

    stack.set(2, "third-set")
    stack.get(2) should be("third-set")

  }

  behavior of "withPointer"
  it should "modify a slice of the stack" in {
    val stack = new ArrayStack[String]()

    stack.push("1")
    stack.push("2")
    stack.push("3") // .
    stack.push("4") // .
    stack.push("5")

    stack.withPointer(2, {
      stack.push("3--")
      stack.push("4--")
    })

    stack.push("6")

    stack.toArray should be(
      Array("1", "2", "3--", "4--", "5", "6")
    )
  }
}