package mini_puck_c.frontend

import mini_puck_c.backend.RuntimeOrganisation.RTLocInfo
import StaticTypes.TypeInfo

/**
  * This object contains the definition of all symbols. A symbol represents a definition and is created when
  * a definition is parsed.
  * Symbols are manged
  *   - in a static environment created when parsing a compilation unit for each parsing process
  *   - in a global name-space if they are exported by some compilation unit.
  */
object ProgSymbols {

  // all entities defined in a program (i.e. names with a compile time value) are symbols
  sealed abstract class ProgSymbol {
    val name: String
  }

  // entities defined in a program that represent a storage location which exists at runtime
  sealed abstract class LocSymbol extends ProgSymbol {
    var staticType: Option[TypeInfo] = None  // will be set by typifier
    var rtLocInfo: Option[RTLocInfo] = None  // will be set by RuntimeLocator
  }

  // Variables denote runtime locations with a direct value
  case class VarSymbol(override val name: String) extends LocSymbol {
    override def toString: String = name + "<VAR"+staticType + ", " + rtLocInfo + ">"
  }

  // Parameters denote runtime locations with a direct or an indirect value
  sealed abstract class ParamSymbol extends LocSymbol

  // Value Parameters denote runtime locations with a direct value
  case class ValParamSymbol(override val name: String) extends ParamSymbol  {
    override def toString: String = name + "<VALP "+staticType + ", " + rtLocInfo + ">"
  }

  // Reference Parameters denote runtime locations with an indirect value
  case class RefParamSymbol(override val name: String) extends ParamSymbol  {
    override def toString: String = name + "<REFP "+staticType + ", " + rtLocInfo + ">"
  }

  //Procedures
  case class ProcSymbol(
    override val name: String,
    var params:      Option[List[ParamSymbol]] = None,
    var locals:      Option[List[VarSymbol]] = None,
  ) extends ProgSymbol


}
