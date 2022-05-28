import collection.mutable.ArrayStack
import org.scalatest.flatspec._
import org.scalatest.matchers._

class ArrayStackSpec extends AnyFlatSpec with should.Matchers {

  it should "parse symbols" in {
    val stack = new ArrayStack[String]()

    stack.push("first")
    stack.push("second")
    stack.length() should be(2)

    stack.pop() should be("second")

    stack.push("third")
    stack.pop() should be("third")
    stack.pop() should be("first")

  }

}