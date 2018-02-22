package mini_puck_c.separate_compilation

import mini_puck_c.frontend.ProgSymbols.ProgSymbol

/**
  *  Instances of this trait represent the global name space (i.e. inter compilation unit name space).
  *  Instances of this trait are used by the compiler when it compiles a compilation unit and encounters
  *  an import or export statement.
  */
trait GlobalNameSpace {

  /**
    * Lookup a symbol given by object name and symbol name
    */
  val importSymbol: PartialFunction[(String, String), ProgSymbol]


  /**
    * Define a symbol given by object name and symbol name
    */
  val exportSymbol: PartialFunction[(String, String, ProgSymbol), Unit]

}
