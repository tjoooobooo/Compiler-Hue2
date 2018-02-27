package frontend
import ProgSymbols._

import scala.collection.mutable


/**
  * This class implements the trait StaticEnv, i.e. a static environment.
  *
  * The environment is managed a stack of frames. Each frame represents a scope.
  * Frames are created and discarded with entering and leaving a scope.
  */
class EnvImpl extends StaticEnv {

  private trait Frame {
    def lookup(name: String) : ProgSymbol
    def define(name: String, value: ProgSymbol): Unit
    def isLocallyDefined(name: String): Boolean
    def isDefined(name: String): Boolean
  }

  // Bottom frame, represents "outside all scopes".
  // Noting is defined, nothing can be defines.
  private case object BottomFrame extends Frame {
    override def lookup(name: String) : ProgSymbol =
      throw new Throwable(s"$name is not defined")
    override def define(name: String, value: ProgSymbol): Unit =
      throw new Throwable(s"$name can not be defined in outmost scope")
    override def isLocallyDefined(name: String): Boolean =
      false
    override def isDefined(name: String): Boolean =
      false
  }

  // represents a scope
  private case class DerivedFrame(base: Frame) extends Frame {
    val entries : mutable.Map[String, ProgSymbol] = mutable.Map() // all names / symbols defined in the scope

    // look here and if not found in outer scopes
    override def lookup(name: String) : ProgSymbol =
      entries.get(name) match {
        case None => base.lookup(name)
        case Some(x) => x
      }

    // define a name in this scope
    override def define(name: String, symb: ProgSymbol): Unit = {
      entries.get(name) match {
        case Some(_) => throw new Throwable("$name already defined")
        case None => entries += (name -> symb)
      }
    }

    // check whether a name is locally defined
    def isLocallyDefined(name: String): Boolean = entries.isDefinedAt(name)

    def isDefined(name: String): Boolean =
      isLocallyDefined(name) || base.isDefined(name)
  }

  // represents the actual scope
  private var actualFrame : Frame = BottomFrame


  def enterScope(): Unit = {
    actualFrame = DerivedFrame(actualFrame)
  }

  def leaveScope(): Unit = {
    actualFrame match {
      case DerivedFrame(previousFrame) =>
        actualFrame = previousFrame
      case _ =>
    }
  }


  val define: PartialFunction[(String, ProgSymbol), ProgSymbol] = {
    case (name, symb) if ! actualFrame.isLocallyDefined(name) =>
      actualFrame.define(name, symb)
      symb
  }

  val defineVariable: PartialFunction[String, VarSymbol] = {
    case name if ! actualFrame.isLocallyDefined(name) =>
      val symb = VarSymbol(name)
      //symb.staticType = Option(IntTypeInfo)  TODO richtigen typ zuordnen?
      actualFrame.define(name, symb)
      symb
  }

  val defineProcedure: PartialFunction[String, ProcSymbol] = {
    case name if ! actualFrame.isLocallyDefined(name) =>
      val symb = ProcSymbol(name)
      actualFrame.define(name, symb)
      symb
  }

  val defineValParam: PartialFunction[String, ValParamSymbol] = {
    case name if ! actualFrame.isLocallyDefined(name) =>
      val symb = ValParamSymbol(name)
      actualFrame.define(name, symb)
      symb
  }

  val defineRefParam: PartialFunction[String, RefParamSymbol] = {
    case name if ! actualFrame.isLocallyDefined(name) =>
      val symb = RefParamSymbol(name)
      actualFrame.define(name, symb)
      symb
  }

  val lookup: PartialFunction[String, ProgSymbol] = {
    case name if actualFrame.isDefined(name) => actualFrame.lookup(name)
  }

}
