import compiler._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SymbolTableSpec extends AnyFlatSpec with should.Matchers {
  it should "define globals" in {
    val global = new SymbolTable()

    val a = global.define("a")
    a should be(new SymbolTable.Symbol(
      index = 0,
      name = "a",
      scope = Global
    ))

    val b = global.define("b")
    b should be(new SymbolTable.Symbol(
      index = 1,
      name = "b",
      scope = Global
    ))
  }

  it should "resolve globals" in {
    val global = new SymbolTable()
    global.define("a")
    global.define("b")

    expectResolveAs(global, ("a", 0, Global))
    expectResolveAs(global, ("b", 1, Global))
  }

  it should "resolve locals" in {
    val global = new SymbolTable()
    global.define("a")
    global.define("b")

    val local = new SymbolTable(Some(global))
    local.define("c")
    local.define("d")

    expectResolveAs(local, ("a", 0, Global))
    expectResolveAs(local, ("b", 1, Global))
    expectResolveAs(local, ("c", 0, Local))
    expectResolveAs(local, ("d", 1, Local))
  }

  it should "resolve nested locals" in {
    val global = new SymbolTable()
    global.define("a")
    global.define("b")

    val firstLocal = new SymbolTable(Some(global))
    firstLocal.define("c")
    firstLocal.define("d")

    val secondLocal = new SymbolTable(Some(firstLocal))
    secondLocal.define("e")
    secondLocal.define("f")

    expectResolveAs(firstLocal, ("a", 0, Global))
    expectResolveAs(firstLocal, ("b", 1, Global))
    expectResolveAs(firstLocal, ("c", 0, Local))
    expectResolveAs(firstLocal, ("d", 1, Local))

    expectResolveAs(secondLocal, ("a", 0, Global))
    expectResolveAs(secondLocal, ("b", 1, Global))
    expectResolveAs(secondLocal, ("e", 0, Local))
    expectResolveAs(secondLocal, ("f", 1, Local))
  }

  def expectResolveAs(table: SymbolTable, data: (java.lang.String, Int, Scope)): Unit = {
    val (name, index, scope) = data
    table.resolve(name) should be(Some(SymbolTable.Symbol(
      index = index,
      name = name,
      scope = scope
    )))
  }
}
