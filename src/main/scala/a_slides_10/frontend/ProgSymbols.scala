package slides_10.frontend
import a_slides_10.frontend.StaticTypes
import a_slides_10.frontend.StaticTypes.TypeInfo
import a_slides_10.backend.RuntimeOrganisation._

object ProgSymbols {

    // all entities defined in a program (i.e. names with a compile time value) are symbols
    sealed abstract class ProgSymbol {
      val name: String
    }

  case class Variable(override val name: String) extends ProgSymbol

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
