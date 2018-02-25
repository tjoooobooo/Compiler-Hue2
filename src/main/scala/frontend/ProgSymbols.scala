package frontend

object ProgSymbols {

  // all entities defined in a program (i.e. names with a compile time value) are symbols
  sealed abstract class ProgSymbol {
    val name: String
  }

  // Variables
  case class Variable(override val name: String) extends ProgSymbol

}
