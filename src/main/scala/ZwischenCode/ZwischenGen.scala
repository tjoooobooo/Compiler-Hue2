package ZwischenCode

import scala.collection.mutable.ListBuffer
import ZwischenAST._
import backend.RuntimeOrganisation
import compiler.AssemblerAST.Outi
import frontend.AST
import frontend.AST._
import frontend.ProgSymbols.VarSymbol
import frontend.StaticTypes.IntTypeInfo

import scala.collection.mutable

object ZwischenGen {


  def genCode(prog: Prog): List[Instr] = {
    // create temporary locations
    var temps: List[Int] = (1 to 31).toList
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

    var offsetCounter = 0
    def newOffSet: Int = {
      offsetCounter += 1
      offsetCounter-1
    }
    var globals : List[VarSymbol] = List()
    //globale Variablen definieren
    for(definition <- prog.defList){
      definition match {
        case VarDef(symb,t,None) =>

          println("bin hier")
          var loc = acquireMIntTemp()
          globalhashMap.put(symb.name, loc)
          globals = globals :+ symb
          defVar(Variable(symb.name,loc))

        case VarDef(symb,t,e) =>
          var loc = acquireMIntTemp()
          globalhashMap.put(symb.name, loc)
          globals = globals :+ symb
          genCodeValExp(e,Variable(symb.name,loc))
        case _ =>
          genCodeProc(definition.asInstanceOf[ProcDef])

      }
    }

    RuntimeOrganisation.topLevelLayout(globals)
    println(globals)
    println("------------------")
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

    def genCodeProc(procDef: ProcDef): Unit = {
      //val procLabel = procSymbToLabel(procDef.symb)
      codeBuf += LabeledInstr(procDef.symb.name + ":")
      // store SP as new FP
      codeBuf += StoreSPasFPInstr
      // allocate non-static locals on stack
      procDef.locals.foreach {
        case VarDef(symb, _, _) =>
          symb.rtLocInfo.get.nesting
          if (symb.rtLocInfo.get.nesting > 0 ) { // deal with stack variables only
            symb.staticType match {
              case Some(IntTypeInfo) =>
                codeBuf += PushMIntInstr(MIntImmediateValue(0))
              case _ => throw new Exception("internal error language supports only int variables")
            }
          }
      }
      // fill locals with init values
      procDef.locals.foreach {
        case VarDef(symb, _, initExp) =>
          val varLoc = MIntFrameLoc(symb.rtLocInfo.get)

          genCodeValExp(initExp, varLoc)
        case _ => // ignore
      }

      // code for body
      procDef.cmds.foreach( cmd => genCode(cmd) )
      // release locals on stack
      procDef.locals.reverse.foreach {
        case VarDef(symb, _, _) =>
          if (symb.rtLocInfo.get.nesting > 0 ) { // deal with stack variables only
            symb.staticType match {
              case Some(IntTypeInfo) =>
                codeBuf += PopMIntInstr
              case _ => throw new Exception("internal error: language supports only int variables")
            }
          }
      }
      // set RR from stack
      codeBuf += PopCodeAddrToRRInstr
      // return to caller
      codeBuf += ReturnInstr
    }

    def defVar(target: MIntLoc): Unit =
      target match {
        case Variable(name, loc) =>
          codeBuf += VarDefInstr(target)
      }

    def genBinOp(l: Exp, op: MOp, r: Exp, target: MIntLoc): Unit = {
      val t1 = acquireMIntTemp()
      genCodeValExp(Option(l), t1)
      val t2 = acquireMIntTemp()
      genCodeValExp(Option(r), t2)
      codeBuf += AssignInstr(target, t1, op, t2)
      releaseMIntTemp(t1)
      releaseMIntTemp(t2)
    }

    def genCodeValExp(exp: Option[Exp], target: MIntLoc): Unit =
        exp.get match {
      case Add(l, r) => genBinOp(l, AddOp, r, target)
      case Sub(l, r) => genBinOp(l, SubOp, r, target)
      case Mul(l, r) => genBinOp(l, MultOp, r, target)
      case Div(l, r) => genBinOp(l, DivOp, r, target)
      case Mod(l, r) => genBinOp(l, ModOp, r, target)
      case And(l, r) => genBinOp(l, AndOp, r, target)
      case Or(l, r) => genBinOp(l, OrOp, r, target)
      case Xor(l, r) => genBinOp(l, XorOp, r, target)
      case Sl(l, r) => genBinOp(l, SlOp, r, target)
      case Sr(l, r) => genBinOp(l, SrOp, r, target)
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
          genCodeValExp(Option(l), t1)
          val t2 = acquireMIntTemp()
          genCodeValExp(Option(r), t2)
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
          genCodeValExp(Option(right), target) // generate code that puts value of right to target

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
        case Write(e) =>
          var tmp = acquireMIntTemp()
          genCodeValExp(Option(e),tmp)
          codeBuf += PrintInteger(tmp)
        case Read(e) =>
          var tmp = acquireMIntTemp()
          genCodeValExp(Option(e),tmp)
          codeBuf += ReadInteger(tmp)
        case Call(symb,args) =>
          println("CALL FOUND")
          println(symb)
          println(args)
          println("-------------")
          codeBuf += PushFPInstr
          //TODO Parameter verarbeiten
          codeBuf += JumpInstr(symb.name)
      }

      genCodeCmd(cmd)
    }
    codeBuf.toList
  }
}
