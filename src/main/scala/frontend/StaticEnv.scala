package frontend
import slides_10.frontend.ProgSymbols._

import scala.util.Try

trait StaticEnv {

  /**
    * Enter a new definition context. All names defined in this context have to be unique.
    * All names defined in this context supersede names defined in previous / outer contexts
    */
  def enterScope(): Unit

  /**
    * Leave the current definition context. All names defined in the current context are invalidated.
    */
  def leaveScope(): Unit

  /**
    * Define a name within the current context
    * @param name the name
    * @param symb the symbol to be associated with the name
    * @return     Success(_) if symbol may be definied in this context
    *             Failure(_) if the symbol may not be defined in this context
    *             (it may already be defined)
    */
  def define(name: String, symb: ProgSymbol): Try[Unit]

  /**
    * Lookup a name within the current context
    * @param name the name to be looked-up
    * @return  Success(symbol) if the symbol that is associated with the name within the current context
    *          Failure(_) if no symbol is associated  with the name within the current context
    */
  def lookup(name: String): Try[ProgSymbol]
}
