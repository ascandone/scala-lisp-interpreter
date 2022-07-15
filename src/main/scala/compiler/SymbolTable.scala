package compiler

import scala.collection.mutable

sealed trait Scope

object Global extends Scope

object Local extends Scope

object Free extends Scope

object SymbolTable {
  case class Identifier(
                         name: java.lang.String,
                         scope: Scope,
                         index: Int
                       )
}

class SymbolTable(val outer: Option[SymbolTable] = None) {
  val freeSymbols = new mutable.ArrayBuffer[SymbolTable.Identifier]()
  private val store = new mutable.HashMap[java.lang.String, SymbolTable.Identifier]()
  private var numDefinitions = 0

  def define(name: java.lang.String, forceGlobal: Boolean = false): SymbolTable.Identifier = {
    if (forceGlobal) {
      outer match {
        case None => define(name)
        case Some(outerTable) => outerTable.define(name, forceGlobal = true)
      }
    } else {
      // TODO use store.size instead of numDefinitions
      val symbol = SymbolTable.Identifier(
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
  }

  def resolve(name: java.lang.String): Option[SymbolTable.Identifier] = store.get(name).orElse {
    outer.flatMap(_.resolve(name)).map(resolvedSymbol => resolvedSymbol.scope match {
      case Global => resolvedSymbol
      case Local | Free => {
        val index = freeSymbols.length
        val symbol = SymbolTable.Identifier(
          name = name,
          scope = Free,
          index = index
        )

        freeSymbols.addOne(resolvedSymbol)
        store.put(name, symbol)
        symbol
      }
    })
  }

  def nested = new SymbolTable(Some(this))
}
