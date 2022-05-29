package compiler

import scala.collection.mutable

sealed trait Scope

object Global extends Scope

object Local extends Scope

object Free extends Scope

object SymbolTable {
  case class Symbol(
                     name: java.lang.String,
                     scope: Scope,
                     index: Int
                   )
}

class SymbolTable(val outer: Option[SymbolTable] = None) {
  val freeSymbols = new mutable.ArrayBuffer[SymbolTable.Symbol]()
  private val store = new mutable.HashMap[java.lang.String, SymbolTable.Symbol]()
  private var numDefinitions = 0

  def define(name: java.lang.String): SymbolTable.Symbol = {
    // TODO use store.size instead of numDefinitions
    val symbol = SymbolTable.Symbol(
      name = name,
      index = numDefinitions,
      scope = outer match {
        case Some(_) => Local
        case None => Global
      }
    )

    numDefinitions += 1

    store(name) = symbol
    symbol
  }

  def resolve(name: java.lang.String): Option[SymbolTable.Symbol] = store.get(name).orElse {
    outer.flatMap(_.resolve(name)).map(resolvedSymbol => resolvedSymbol.scope match {
      case Global => resolvedSymbol
      case Local | Free => {
        val index = freeSymbols.length
        val symbol = SymbolTable.Symbol(
          name = name,
          scope = Free,
          index = index
        )

        freeSymbols.addOne(resolvedSymbol)
        store.put(name, resolvedSymbol)
        symbol
      }
    })
  }

  def nested = new SymbolTable(Some(this))
}
