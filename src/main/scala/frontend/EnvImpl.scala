package frontend
import scala.collection.mutable
import scala.util.{Failure, Try}
import slides_10.frontend.ProgSymbols._
class EnvImpl extends StaticEnv {

  private trait Frame {
    def lookup(name: String) : ProgSymbol
    def define(name: String, value: ProgSymbol): Unit
  }

  // Bottom frame, represents "outside all scopes".
  // Noting is defined, nothing can be defines.
  private case object BottomFrame extends Frame {
    override def lookup(name: String) : ProgSymbol =
      throw new Throwable(s"$name is not defined")
    override def define(name: String, value: ProgSymbol): Unit =
      throw new Throwable(s"$name can not be defined in outmost scope")
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

  def define(name: String, symb: ProgSymbol): Try[Unit] = Try {
    actualFrame.define(name, symb)
  } recoverWith {
    case e: Throwable => Failure(e)
  }

  def lookup(name: String) : Try[ProgSymbol] = Try {
    actualFrame.lookup(name)
  } recoverWith {
        case e: Throwable => Failure(e)
    }

}
