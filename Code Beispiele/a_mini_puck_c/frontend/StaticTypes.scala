package mini_puck_c.frontend

/**
  * This object contains the definition of all internal representations of types of the source language.
  */
object StaticTypes {

  /**
    * Information about a type in a source program.
    * A TypeInfo is / represents the semantics of type expressions and
    * the implicit types of reference parameters.
    */
  sealed abstract class TypeInfo

  /**
    * Represents an Int type.
    */
  object IntTypeInfo extends TypeInfo  {
    override def toString: String = "INT"
  }

  /**
    * Represents a reference type, i.e. the type of a ref-parameter.
    */
  case class RefTypeInfo(baseType: TypeInfo) extends TypeInfo  {
    override def toString: String = "REF-to-" + baseType.toString
  }

}
