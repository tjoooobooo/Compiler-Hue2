package mini_puck_c.backend

import CodeGenerator.IntermediateInstr

/**
  * A code builder accepts a sequence of intermediate code
  * instructions and directives and transforms them to some "real" linkable code
  * that may be a (direct or indirect) input of some linker.
  */
trait CodeBuilder {

  /**
    * Directives are used to steer the code generation and / or the linker.
    * Depending on the target system the compiler has or has not to mangle names, i.e. to
    * transform local names (of addresses) in some way. So directives may be interpreted
    * in part or completely by the actual LinkUnitBuilder (i.e. within the compiler) or
    * they are just transformed to linker directives (which are interpreted by the linker).
    */
  sealed abstract class Directive
  case class CuNameDirective(cuName: String) extends Directive
  case class ImportDirective(objName: String, name: String) extends Directive
  case class ExportDirective(name: String) extends Directive

  /**
    * Attaches instructions of the target language that correspond to an
    * intermediate code instruction to the code buffer.
    * @param instr the intermediate code instruction that is to be translated to real code
    */
  def += (instr: IntermediateInstr): Unit

  /**
    * Get the generated code as string.
    * @return the code generated up to the moment of calling this method.
    */
  def getCode: String


  /**
    * Pass a directive to the builder.
    * Directives are not translated to code, they direct the linking processes and / or the process of
    * code generation in case the compiler has to perform some kind of name mangling.
    * @param directive
    */
  def direct(directive: Directive)

}
