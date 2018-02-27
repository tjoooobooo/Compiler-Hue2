package mini_puck_c.frontend

import mini_puck_c.frontend.ProgSymbols._

/**
  * Manage name resolution (definition and lookup) in a scoped environment.
  *
  * Definition and lookpup are defined as partial functions to allow for a smooth cooperation with parsers.
 */
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
    * Define a name ~> Symbol with a known symbol binding within the current context if it isn't already defined
    */
  val define: PartialFunction[(String, ProgSymbol), ProgSymbol]

  /**
    * Define a name ~> Symbol binding within the current context if it isn't already defined
    */
  val defineVariable: PartialFunction[String, VarSymbol]

  /**
    * Define a name ~> Symbol binding within the current context if it isn't already defined
    */
  val defineProcedure: PartialFunction[String, ProcSymbol]

  /**
    * Define a name ~> Symbol binding within the current context if it isn't already defined
    */
  val defineValParam: PartialFunction[String, ValParamSymbol]

  /**
    * Define a name ~> Symbol binding within the current context if it isn't already defined
    */
  val defineRefParam: PartialFunction[String, RefParamSymbol]

  /**
    * Lookup a name in the current environment if the name is defined
    */
  val lookup: PartialFunction[String, ProgSymbol]
}
