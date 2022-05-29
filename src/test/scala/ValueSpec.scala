import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._

class ValueSpec extends AnyFlatSpec with should.Matchers {
  it should "override toString" in {
    Number(0).show should be("0")
    String("hello").show should be("\"hello\"")
    Symbol("hello").show should be("hello")
    List.of().show should be("nil")
    List.of(
      Symbol("a"),
      List.of(),
      Symbol("c"),
    ).show should be("(a nil c)")
  }
}
