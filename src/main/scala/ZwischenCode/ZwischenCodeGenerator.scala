package ZwischenCode

import ZwischenCode.ZwischenCodeAST._
import backend.RuntimeOrganisation
import frontend.AST._
import frontend.ProgSymbols.{ProcSymbol, VarSymbol}
import frontend.StaticTypes.IntTypeInfo

import scala.collection.mutable.ListBuffer

object ZwischenCodeGenerator {

  val codeBuf: ListBuffer[IntermediateInstr] = new ListBuffer()
    // -------------------------------------------------------------------------------------------------------------------
    // Definition of intermediate code
    private val tempsCount = 25 // number of temporary locations (registers) for MInt and MAddress each
    // create temporary locations
    private var tempsMInt: List[Int] = (1 to tempsCount).toList

    // get next unused MInt temporary
    def acquireMIntTemp(): TempMIntLoc = {
      val res = TempMIntLoc(tempsMInt.head)
      tempsMInt = tempsMInt.tail
      res
    }

    // release a MInt temporary
    def releaseMIntTemp(t: TempMIntLoc) : Unit = {
      tempsMInt = t.nr :: tempsMInt
    }

    // create temporary address locations
    private var tempsMAddress: List[Int] = (0 to tempsCount).toList

    // get next unused temporary
    private def acquireMAddressTemp(): TempMAddressLoc = {
      val res = TempMAddressLoc(tempsMInt.head)
      tempsMAddress = tempsMAddress.tail
      res
    }

    // release an address temporary
    private def releaseMAddressTemp(t: TempMAddressLoc) : Unit = {
      tempsMAddress = t.nr :: tempsMAddress
    }


    // Create labels
    // labels are craeted such that they unique with a compilation unit.
    private var labelCount = 0
    private def newLabel : String = {
      labelCount = labelCount + 1
      "L_"+labelCount
    }



    // -------------------------------------------------------------------------------------------------------------------
    // Definition of code generating procedures
    // codeBuf is used to actually create code


    /*
     * Generate code for an expression.
     * @param exp  the expression to be translated
     */
    private def genCodeValExp(exp: Exp, target: MIntLoc): Unit = {

      def genBinOp(l: Exp, op:MOp, r: Exp, target: MIntLoc): Unit = {
        val t1 = acquireMIntTemp()
        genCodeValExp(l, t1)
        val t2 = acquireMIntTemp()
        genCodeValExp(r, t2)
        codeBuf += AssignInstr(target, t1, op, t2)
        releaseMIntTemp(t1)
        releaseMIntTemp(t2)
      }

      exp match {
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
        case LocAccess(locExp) =>
          val t = genCodeIntLocExp(locExp) // put location denoted by locExp into t
          if (t != target) { // avoid assignment to itself
            codeBuf += AssignInstr(target, t)
          }
        case n@Number(_) =>
          codeBuf += AssignInstr(target, MIntImmediateValue(n.d))
        case _ =>
      }
    }


    /* Generates code that jumps to a given label if the boolean expression evaluates to true.
     * Parameters:
     *   - bExp: the boolean expression, i.e. a comparison
     *   - trueLabel: the label to jump to if the comparison evaluates to true
     */
    def genCodeBe(bExp: BoolExp, trueLabel: String): Unit = {
      def genCondJump(l: Exp, r: Exp, compOp: MRelOp) : Unit = {
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


    /*
     * Generate code for a cmd.
     * @param cmd the cmd
     */
    private def genCodeCmd(cmd: Cmd): Unit = cmd match {
      case Assign(left, right) =>
        val target = genCodeIntLocExp(left) // target location now contains the destination loc
        genCodeValExp(right, target)         // generate code that puts value of right to target

      case If(cond, thenCmds, Nil) =>
        val thenLabel = newLabel
        val exitLabel = newLabel
        genCodeBe(cond, thenLabel)
        codeBuf += JumpInstr(exitLabel)
        codeBuf += LabeledInstr(thenLabel)
        thenCmds.foreach { cmd => genCodeCmd(cmd) }
        codeBuf += LabeledInstr(exitLabel)

      case If(cond, thenCmds, elseCmds) =>
        val thenLabel = newLabel
        val exitLabel = newLabel
        genCodeBe(cond, thenLabel)
        elseCmds.foreach { cmd => genCodeCmd(cmd) }
        codeBuf += JumpInstr(exitLabel)
        codeBuf += LabeledInstr(thenLabel)
        thenCmds.foreach { cmd => genCodeCmd(cmd) }
        codeBuf += LabeledInstr(exitLabel)

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

      case Call(symb, args) => symb match {
        case pSymb@ProcSymbol(_, Some(_), Some(_)) =>

          codeBuf += PushFPInstr   // push actual frame pointer

          // push arguments
          args.foreach {  // put arguments on stack
            case Arg(rExp, Some(ByValue)) => // pass by value
              val t = acquireMIntTemp()
              genCodeValExp(rExp, t)
              codeBuf += PushMIntInstr(t)
              releaseMIntTemp(t)
            case Arg(lExp, Some(ByRef)) => // pass by reference
              lExp match {
                case LocAccess(locExp) =>
                  val intLoc = genCodeIntLocExp(locExp)
                  val a = acquireMAddressTemp()
                  codeBuf += AssignAddrInstr(a, MkRef(intLoc))
                  codeBuf += PushMAddressInstr(a)
                  releaseMAddressTemp(a)
                case _ => throw new Exception(s"internal error: illegal passing of value argument by ref")
              }
            case arg@Arg(_, None) => throw new Exception(s"internal error: malformed argument $arg")
          }

          val returnLabel = newLabel // label behind call instruction

          // push return address
          codeBuf += PushCodeAddrInstr(returnLabel)

          codeBuf += CallInstr(procSymbToLabel(pSymb))  // call procedure
          codeBuf += LabeledInstr(returnLabel) //TODO label richtig?

          // clean stack in reverse order
          // return address has already been poped by called procedure
          args.foreach { // pop arguments
            case Arg(_, Some(ByValue)) =>
              codeBuf += PopMIntInstr
            case Arg(_, Some(ByRef)) =>
              codeBuf += PopMAddressInstr
            case arg@Arg(_, None) => throw new Exception(s"internal error: malformed argument $arg")
          }

          // restore frame pointer
          codeBuf += PopFPInstr

        case _ => throw new Exception(s"Procedure expected but ${symb.name} found")
      }

      case Write(e) =>
        val t = acquireMIntTemp()
        genCodeValExp(e, t)
        codeBuf += WriteInstr(t)
      case Read(e) =>
        var tmp = acquireMIntTemp()
        genCodeValExp(e,tmp)
        codeBuf += ReadInstr(tmp)
    }

    /*
     * Generate code for a int l-Expression, i.e. an expression that denotes a location containing an int value.
     * @param locExp the expression to be translated
     * @return       the (compile-time description of the) location denoted by the expression.
     */
    private def genCodeIntLocExp(locExp: LocExp): MIntLoc = locExp match {
      case  DirectLoc(locSymb) =>
        MIntProgLoc(locSymb.rtLocInfo.get)
      case StarConv(subLocExp) =>
        val addrLoc = acquireMAddressTemp()
        genCodeAddrLocExp(subLocExp, addrLoc)  // put the address denoted by subLocExp into addrLoc
        DeRef(addrLoc)
    }


    /*
     * Generate code for a address l-Expression, i.e. an expression that denotes a location containing an address value.
     * @param locExp the expression to be translated
     * @return       the (compile-time description of the) location denoted by the expression.
     */
    private def genCodeAddrLocExp(locExp: LocExp, target: MAddressLoc): Unit = locExp match {
      case  DirectLoc(locSymb) =>
        codeBuf += AssignAddrInstr(target, MAddressProgLoc(locSymb.rtLocInfo.get))
      case StarConv(_) =>
        throw new Throwable("Pointer to pointer is ist not a legal type")
    }


    // create a name of a defined procedure
    private def procSymbToLabel(ps: ProcSymbol) : String = ps.name


    /*
     * Generate code for a procedure.
     * @param procDef the definition of the procedure.
     */
    private def genCodeProc(procDef: ProcDef): Unit = {
      val procLabel = procSymbToLabel(procDef.symb)
      codeBuf += ProcEntryInstr(procLabel)

      // store SP as new FP
      codeBuf += StoreSPasFPInstr

      // allocate non-static locals on stack
      procDef.locals.foreach {
        case VarDef(symb, _, _) =>
          if (symb.rtLocInfo.get.nesting > 0 ) { // deal with stack variables only
            symb.staticType match {
              case Some(IntTypeInfo) =>
                codeBuf += PushMIntInstr(MIntImmediateValue(0))
              case _ => throw new Exception("internal error language supports only int variables")
            }
          }
      }

      // code for body
      procDef.cmds.foreach( cmd => genCodeCmd(cmd) )

      // release locals on stack
      procDef.locals.reverse.foreach {
        case VarDef(symb, _, _) =>
          if (symb.rtLocInfo.get.nesting > 0 ) { // deal with stack variables only
            symb.staticType match {
              case Some(IntTypeInfo) =>
                codeBuf += PopMIntInstr
              case _ => throw new Exception("internal error language supports only int variables")
            }
          }
      }

      // set RR from stack
      codeBuf += PopCodeAddrToRRInstr

      // return to caller
      codeBuf += ReturnInstr

    }


    /*
     * Generate code for a global variable.
     * @param varDef the definition of the global variable.
     */
    private def genCodeGlobVarDef(varDef: VarDef): Unit = varDef match {
      case VarDef(varSymb, _, _) =>
        varSymb.staticType match {
          case Some(IntTypeInfo) =>
            codeBuf += AllocStaticInstr(RuntimeOrganisation.addressableCellsPerInt)
          case _ => throw new Exception(s"internal error: symbol ${varSymb.name} should be of type int")
        }
    }

  def translate(obj: Obj): List[IntermediateInstr] = {
    codeBuf += ObjectInstr(obj.name)
    var globalList : ListBuffer[VarSymbol] = new ListBuffer[VarSymbol]
    obj.defs foreach{
      case varDef@VarDef(symb,t,e) => globalList += symb
      case _ =>
    }
    RuntimeOrganisation.topLevelLayout(globalList.toList)
    obj.defs foreach{
      case varDef@VarDef(_,_,_) => genCodeGlobVarDef(varDef)
      case _ =>
    }
    obj.defs foreach{
      case procDef@ProcDef(_, _, _, _) => genCodeProc(procDef)
      case _ =>
    }


    codeBuf += ProcEntryInstr("main")
    obj.cmds.foreach{genCodeCmd}


    codeBuf.toList
  }
}
