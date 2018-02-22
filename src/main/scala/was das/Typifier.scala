object Typifier {

  def typify(prog: Prog): Unit = {
    typeDefs(prog.defs)
    typifyCmds(prog.cmds)
  }

  private def typifyDefs(defs: List(Definition)) : Unit = {
    defs.foreach {
      case VarDef{ }
    }
  }
}
