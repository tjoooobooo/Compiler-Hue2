package mini_puck_c.backend

/**
  * Instances of this class are used to create a textual representation of the intermediate code.
  */
class IMCodeBuilder extends CodeBuilder {

  private val strBuf: StringBuilder = new StringBuilder

  override def getCode: String = strBuf.mkString

  override def +=(instr: CodeGenerator.IntermediateInstr): Unit =
    strBuf.append(instr.toString())

  override def direct(directive: Directive): Unit = {}
}
