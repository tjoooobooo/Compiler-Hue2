package compiler

import frontend.AST._

import scala.util.{Failure, Success, Try}

object AstTraversal {
  /**
    * Helper for the traversal of all definitions of a compilation unit (an object).
    *
    * @param prog the object the definitions of which are to be traversed.
    */
  case class AllDefs(prog: Prog) {

    private val nestedDefs: Definition => Seq[Definition] = {
      case ProcDef(_, paramdefs, localdefs, _) =>
        paramdefs ++ localdefs
      case _ => Nil
    }

    private def allDefs(prog: Prog): Seq[Definition] = {
      prog.defList ++ prog.defList.flatMap(nestedDefs)
    }

    /**
      * Traverse AST and perfon an action on all definitions.
      * @param action the action to be performed
      * @return       Success of all or failure of at least one action.
      */
    def traverseWith(action: Definition => Unit): Try[Unit] =
      allDefs(prog).foldLeft(Success(()): Try[Unit]) {
        case (Success(()), x) => Try {
          action(x)
        }
        case (Failure(t), _) => Failure(t)
      }
  }

  /**
    * Helper for the traversal of all commands of a compilation unit (an object).
    *
    * @param prog the object the commands of which are to be traversed.
    */
  case class AllCmds(prog: Prog) {

    private val nestedCmds: Cmd => Seq[Cmd] = {
      case c@If(_, thenCmds, elseCmds) => Seq(c) ++ thenCmds.flatMap(nestedCmds) ++ elseCmds.flatMap(nestedCmds)
      case w@While(_, cmds) => Seq(w) ++ cmds.flatMap(nestedCmds)
      case c => Seq(c)
    }

    private val nestedCmdsInDef : Definition => Seq[Cmd] = {
      case ProcDef(_, _, _, cmds) =>
        cmds.flatMap(nestedCmds)
      case _ => Nil
    }

    private def allCmds(prog: Prog): Seq[Cmd] = {
      prog.cmdList ++ prog.defList.flatMap(nestedCmdsInDef)
    }

    /**
      * Traverse AST and perfon an action on all commands.
      * @param action the action to be performed
      * @return       Success of all or failure of at least one action.
      */
    def traverseWith(action: Cmd => Unit): Try[Unit] =
      allCmds(prog).foldLeft(Success(()): Try[Unit]) {
        case (Success(()), x) => Try {
          action(x)
        }
        case (Failure(t), _) => Failure(t)
      }
  }

}



