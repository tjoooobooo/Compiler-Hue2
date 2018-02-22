package slides_10.backend

abstract class RuntimeEnv {
  import collection.mutable

  type Address

  private trait Frame {
    def lookup(name: String) : Address
    def define(name: String, adr: Address): Unit
  }

  private case object BottomFrame extends Frame {
    override def lookup(name: String) : Address = throw new NoSuchElementException(name + " in Env")
    override def define(name: String, adr: Address): Unit = throw new UnsupportedOperationException
  }

  private case class DerivedFrame(base: Frame) extends Frame {
    val entries : mutable.Map[String, Address] = mutable.Map()

    override def lookup(name: String) : Address =
      try {
        entries(name)
      } catch {
        case _: NoSuchElementException => base.lookup(name)
      }

    override def define(name: String, value: Address): Unit = {
      entries += (name -> value)
    }
  }

  private var actualFrame : Frame = BottomFrame

  def enterScope(): Unit = { actualFrame = new DerivedFrame(actualFrame) }

  def leaveScope(): Unit = {
    actualFrame match {
      case DerivedFrame(previousFrame) => {
        actualFrame = previousFrame
      }
      case _ => throw new Exception("leaving base frame")
    }
  }

  def define(name: String, adr: Address): Unit = actualFrame.define(name, adr)
  def lookup(name: String) : Address = actualFrame.lookup(name)

}
