package thm.mni.cb1718.hue_0.compiler

import thm.mni.cb1718.hue_0.{ExpTree, Parser}

object Compiler {

  def compile(prog: String): List[Instruction] = {
      val tree: ExpTree = Parser.parse(prog)
      val code: List[Instruction] = CodeGen.genCode(tree)
      code
  }


}
