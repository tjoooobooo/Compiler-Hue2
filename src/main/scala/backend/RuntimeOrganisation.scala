package backend

import frontend.ProgSymbols.{ParamSymbol, ProcSymbol, VarSymbol}
import frontend.StaticTypes.{IntTypeInfo, RefTypeInfo, TypeInfo}

/**
  * This object contains definitions, constants and functions that are related to the runtime organisation of the
  * target machine.
  */
object RuntimeOrganisation {

  /**
    * Info that is attached to compile time entities, that represent locations at runtime,
    * i.e. variables and parameters
    * @param nesting the nesting level of location: global = 0, local to a procedure = 1
    * @param offset  the offset of the location in a frame (local name) or a static data area (global)
    */
  case class RTLocInfo (nesting: Int, offset: Int)


  /**
    * Number of memory cells (addressable units) a pointer needs.
    */
  val addressableCellsPerInt: Int     = 1

  /**
    * Number of memory cells (addressable units) an int value needs.
    */
  val addressableCellsPerPointer: Int = 1


  // position of local variables and parameters within a frame:
  // parameters are located at increasing addresses starting at paramsStart
  // locals are located at decreasing addresses starting at localsStart
  private val paramsStart  = 2 // distance of parameters from Stackpointer SP
  private val localsStart  = 0 // distance of local variables from Stackpointer SP


  /*
   * Compute the number of addressable storage cells for values of a given type
   * @param typ  the type of the value
   * @return     the number of storage cells needed to store the value
   */
  private def cellClaim(typ: TypeInfo): Int = typ match {
    case IntTypeInfo    => addressableCellsPerInt
    case RefTypeInfo(_) => addressableCellsPerPointer
  }

  /**
    * Assign a frame position to all parameters and variables of the procedure.
    * @param nestingLevel  the nesting level of the procedure
    * @param symbol        the procedure's symbol
    */
  def frameLayout(nestingLevel: Int, symbol: ProcSymbol): Unit = {
    var actDownOffset: Int = localsStart // keeps track of offset for local variables
    var actUpOffset: Int   = paramsStart // keeps track of offsets for parameters

    def nextUpOffset(cellClaim: Int): Int = { // to be called for parameters in reverse order
      val offset = actUpOffset
      actUpOffset = actUpOffset + cellClaim
      offset
    }

    def nextDownOffset(cellClaim: Int): Int = { // to be called for locals, then for all levels of display
      val offset = actDownOffset
      actDownOffset = actDownOffset - cellClaim
      offset
    }

    symbol.params match {
      case None => throw new Exception(s"internal errror, uninitilized parameter symbols of proc $symbol")

      case Some(paramSymbols) =>
        paramSymbols.foreach((ps: ParamSymbol) => {
          val offset = nextUpOffset(cellClaim(ps.staticType.get))
          ps.rtLocInfo = Some(RTLocInfo(nestingLevel, offset))
        })
    }

    symbol.locals match {
      case None => throw new Exception(s"internal errror, uninitilized parameter symbols of proc $symbol")
      case Some(varSymbols) =>
        varSymbols.foreach ( (vs: VarSymbol) => {
          val offset = nextDownOffset(cellClaim(vs.staticType.get))
          vs.rtLocInfo = Some(RTLocInfo(nestingLevel, offset))
        })
    }
  }

  /**
    * Assigns a position to all global variables.
    *
    * @param varSymbols the variables
    */
  def topLevelLayout(varSymbols: List[VarSymbol]): Unit = {
    var actDownOffset: Int = localsStart
    def nextDownOffset(cellClaim: Int): Int = { // to be called for locals, then for all levels of display
      val offset = actDownOffset
      actDownOffset = actDownOffset - cellClaim
      offset
    }
    varSymbols.foreach ( (vs: VarSymbol) => {
      val offset = nextDownOffset(cellClaim(vs.staticType.get))
      vs.rtLocInfo = Some(RTLocInfo(0, offset))
    })

  }

}
