import org.scalatest.flatspec._
import org.scalatest.matchers._
import value._

class ValueSpec extends AnyFlatSpec with should.Matchers {
  it should "override toString" in {
    Number(0).toString should be("0")
    String("hello").toString should be("\"hello\"")
    Symbol("hello").toString should be("hello")
    List.of().toString should be("nil")
    List.of(
      Symbol("a"),
      List.of(),
      Symbol("c"),
    ).toString should be("(a nil c)")
  }
}
