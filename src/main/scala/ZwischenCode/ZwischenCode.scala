package ZwischenCode

import scala.collection.mutable.ListBuffer
import ZwischenAST._
import frontend.AST._

object ZwischenCode {

// TODO macht scheiße
  def gen(cmd : Cmd) : List[Instr] = {
    var temps: List[Instr] = List()
    temps
  }

  def genCode(exp: Cmd) : List[Instr] = {
    // create temporary locations
    var temps: List[Int] = (0 to 5).toList
    // get next unused temporary
    def acquireTemp(): TempLoc = {
      val res = TempLoc(temps.head)
      temps = temps.tail
      res
    }

    def releaseTemp(t: TempLoc) : Unit = {
      temps = t.nr :: temps
    }
    val codeBuf : ListBuffer[Instr] = new ListBuffer()

    def genBinOp(l: Exp, op:MOp, r: Exp, target: Location): Unit = {
      val t1 = acquireTemp()
      genCodeValExp(l, t1)
      val t2 = acquireTemp()
      genCodeValExp(r, t2)
      codeBuf += AssignInstr(target, Option(t1), Option(op), Option(t2))
      releaseTemp(t1)
      releaseTemp(t2)
    }
    def genCodeCmd(cmd: Cmd): Unit = cmd match {
      case Assign(left, right) =>
        val target = acquireTemp() // target location now contains the the destination loc
        genCodeValExp(right, target) // generate code that puts value of right to target
    }
    def genCodeValExp(exp: Exp, target: Location): Unit = exp match {
      case Add(l, r) => genBinOp(l, AddOp, r, target)
      case Sub(l, r) => genBinOp(l, SubOp, r, target)
      case Mul(l, r) => genBinOp(l, MultOp, r, target)
      case Div(l, r) => genBinOp(l, DivOp, r, target)
      case Number(v) =>
        codeBuf += AssignInstr(target, MIntImmediateValue(v))
    }

    var t = acquireTemp()
    genCodeCmd(exp)
    codeBuf.toList
  }

  def genCodeAll(prog : Prog) : List[Instr] = {
    var temps: List[Instr] = List()
    // Variablen TODO in deflist sind proceduren und da können auch cmds sein
    prog.defList
    // CMD'S
    prog.cmdList
    temps
  }

}
