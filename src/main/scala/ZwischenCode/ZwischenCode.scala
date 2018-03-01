package ZwischenCode

import scala.collection.mutable.ListBuffer
import ZwischenAST._
import backend.Evaluator.store
import backend.{RuntimeEnv, Store}
import frontend.AST
import frontend.AST._
import frontend.ProgSymbols.VarSymbol

import scala.collection.mutable

object ZwischenCode {
  // TODO addressenMIST
  private val store = new Store
  private val env = new RuntimeEnv {
    override type Address = store.Address
  }

  def genCodeAll(prog : Prog) : List[Instr] = {
    val codeBuf : ListBuffer[Instr] = new ListBuffer()


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

    // create labels
    var labelCount = 0
    def newLabel : String = {
      labelCount = labelCount + 1
      "L_"+labelCount
    }
  //TODO ADDRESSEN interface velleicht
    def genCodeIntLocExp(ref: RefExp) : Location =  ref match{
      case VarRef2(varsymb) => env.lookup(varsymb.name) match {
        case _: NoSuchElementException => env.define(varsymb.name)

      }

    }

    def genCodeCmd(cmd: Cmd) : Unit = cmd match {
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

    }

    /* Generates code that jumps to a given label if the boolean expression evaluates to true.
   * Parameters:
   * - bExp: the boolean expression, i.e. a comparison
   * - trueLabel: the label to jump to if the comparison evaluates to true
   */
    def genCodeBe(bExp: BoolExp, trueLabel: String): Unit = {
      def genCondJump(l: Exp, r: Exp, compOp: MRelOp) : Unit = {
        val t1 = acquireTemp
        genCodeValExp(l, t1)
        val t2 = acquireTemp
        genCodeValExp(r, t2)
        codeBuf += IfInstr(t1, compOp, t2, trueLabel)
        releaseTemp(t1)
        releaseTemp(t2)
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

    def genBinOp(l: Exp, op:MOp, r: Exp, target: Location): Unit = {
      val t1 = acquireTemp()
      genCodeValExp(l, t1)
      val t2 = acquireTemp()
      genCodeValExp(r, t2)
      codeBuf += AssignInstr(target, Option(t1), Option(op), Option(t2))
      releaseTemp(t1)
      releaseTemp(t2)
    }

    def genCodeValExp(exp: Exp, target: Location): Unit = exp match {
      case Add(l, r) => genBinOp(l, AddOp, r, target)
      case Sub(l, r) => genBinOp(l, SubOp, r, target)
      case Mul(l, r) => genBinOp(l, MultOp, r, target)
      case Div(l, r) => genBinOp(l, DivOp, r, target)
      case Number(v) =>
        codeBuf += AssignInstr(target, MIntImmediateValue(v))
    }
    /*var t = acquireTemp()
    genCodeExp(exp)*/

    // Variablen TODO in deflist sind proceduren und da können auch cmds sein
    prog.defList
    // CMD'S
    prog.cmdList.foreach{genCodeCmd}

    codeBuf.toList
  }

}
