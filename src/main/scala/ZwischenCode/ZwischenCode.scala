package ZwischenCode

import scala.collection.mutable.ListBuffer
import ZwischenAST._
import frontend.AST
import frontend.AST._

import scala.collection.mutable
import scala.util.Try


object ZwischenCode {


  def genCode(prog: Prog): List[Instr] = {
    // create temporary locations TODO tempsCount
    var temps: List[Int] = (0 to 5).toList
    val codeBuf: ListBuffer[Instr] = new ListBuffer()
    var globalhashMap : mutable.HashMap[String,MIntLoc] = new mutable.HashMap()

    def acquireMIntTemp(): TempMIntLoc = {
      val res = TempMIntLoc(temps.head)
      temps = temps.tail
      res
    }

    def releaseMIntTemp(t: TempMIntLoc): Unit = {
      temps = t.nr :: temps
    }

    //globale Variablen definieren
    for(definition <- prog.defList){
      definition match {
        case VarDef(symb,t,e) =>
          var loc = acquireMIntTemp()
          globalhashMap.put(symb.name, loc)
          genCodeValExp(e,Variable(symb.name,loc))
        case ProcDef(symb,fparams,locals,cmds) =>
          // TODO proceduren

      }
    }
    prog.cmdList.foreach{genCode}

    def genCodeIntLocExp(exp: AST.LocExp): MIntLoc= exp match {
      case DirectLoc(symb) =>
        globalhashMap.get(symb.name) match{
          case Some(loc) => Variable(symb.name,loc)
          case None => Nil
            //eig exception
            acquireMIntTemp()
        }
      case StarConv(locExp) => acquireMIntTemp()
    }


    def genBinOp(l: Exp, op: MOp, r: Exp, target: MIntLoc): Unit = {
      val t1 = acquireMIntTemp()
      genCodeValExp(l, t1)
      val t2 = acquireMIntTemp()
      genCodeValExp(r, t2)
      codeBuf += AssignInstr(target, t1, op, t2)
      releaseMIntTemp(t1)
      releaseMIntTemp(t2)
    }

    def genCodeValExp(exp: Exp, target: MIntLoc): Unit = exp match {
      case Add(l, r) => genBinOp(l, AddOp, r, target)
      case Sub(l, r) => genBinOp(l, SubOp, r, target)
      case Mul(l, r) => genBinOp(l, MultOp, r, target)
      case Div(l, r) => genBinOp(l, DivOp, r, target)
      case Number(v) =>
        codeBuf += AssignInstr(target, MIntImmediateValue(v))
      case LocAccess(locExp) =>
        var v = genCodeIntLocExp(locExp)
          codeBuf += AssignInstr(target,v)
    }

    def genCode(cmd: Cmd): Unit = {

      //var t = acquireMIntTemp()
      //genCodeValExp(exp,t)

      // Create labels
      var labelCount = 0

      def newLabel: String = {
        labelCount = labelCount + 1
        "L_" + labelCount
      }

      /* Generates code that jumps to a given label if the boolean expression evaluates to true.
       * Parameters:
       * - bExp: the boolean expression, i.e. a comparison
       * - trueLabel: the label to jump to if the comparison evaluates to true
       */
      def genCodeBe(bExp: BoolExp, trueLabel: String): Unit = {
        def genCondJump(l: Exp, r: Exp, compOp: MRelOp): Unit = {
          val t1 = acquireMIntTemp()
          genCodeValExp(l, t1)
          val t2 = acquireMIntTemp()
          genCodeValExp(r, t2)
          codeBuf += IfInstr(t1, compOp, t2, trueLabel)
          releaseMIntTemp(t1)
          releaseMIntTemp(t2)
        }

        bExp match {
          case Less(l: Exp, r: Exp) =>
            genCondJump(l, r, LsOp)
          case Greater(l: Exp, r: Exp) =>
            genCondJump(l, r, GtOp)
          case Equal(l: Exp, r: Exp) =>
            genCondJump(l, r, EqOp)
          case LessEq(l: Exp, r: Exp) =>
            genCondJump(l, r, LeOp)
          case GreaterEq(l: Exp, r: Exp) =>
            genCondJump(l, r, GeOp)
          case NotEq(l: Exp, r: Exp) =>
            genCondJump(l, r, NeOp)
        }
      }

      def genCodeCmd(cmd: Cmd): Unit = cmd match {
        case Assign(left, right) =>
          val target = genCodeIntLocExp(left) // target location now contains the the destination loc
          genCodeValExp(right, target) // generate code that puts value of right to target

        case While(cond, body) =>
          val startLabel = newLabel
          val continueLabel = newLabel
          val endLabel = newLabel
          codeBuf += LabeledInstr(startLabel)
          genCodeBe(cond, continueLabel)
          codeBuf += JumpInstr(endLabel)
          codeBuf += LabeledInstr(continueLabel)
          body.foreach { cmd => genCodeCmd(cmd) }
          codeBuf += JumpInstr(startLabel)
          codeBuf += LabeledInstr(endLabel)
        // Einarmiges IF
        case If(cond, thenCmds, Nil) =>
          val thenLabel = newLabel
          val exitLabel = newLabel
          genCodeBe(cond, thenLabel)
          codeBuf += JumpInstr(exitLabel)
          codeBuf += LabeledInstr(thenLabel)
          thenCmds.foreach { cmd => genCodeCmd(cmd) }
          codeBuf += LabeledInstr(exitLabel)
        // Zweiarmiges IF
        case If(cond, thenCmds, elseCmds) =>
          val thenLabel = newLabel
          val elseLabel = newLabel
          val exitLabel = newLabel
          genCodeBe(cond, thenLabel)
          elseCmds.foreach { cmd => genCodeCmd(cmd) }
          codeBuf += JumpInstr(exitLabel)
          codeBuf += LabeledInstr(thenLabel)
          thenCmds.foreach { cmd => genCodeCmd(cmd) }
          codeBuf += LabeledInstr(exitLabel)

      }
      genCodeCmd(cmd)
    }
    codeBuf.toList
  }
}
