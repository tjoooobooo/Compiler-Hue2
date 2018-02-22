package mini_puck_c.backend

import mini_puck_c.compiler.AST._
import RuntimeOrganisation.RTLocInfo
import mini_puck_c.frontend.ProgSymbols.ProcSymbol
import mini_puck_c.frontend.StaticTypes.IntTypeInfo



/**
  * Companion object of the code generator class.
  * Contains
  *  - the definition of the intermediate code and its components.
  *  - the code builder class wich actually generates code.
  */
object CodeGenerator {

  // Locations and values ----------------------------------------------------------------------------------------------

  //----------------------------------------------------
  // int values and locations

  // int value or a location with an int value
  sealed abstract class MIntLocOrValue

  // a location with an int value
  sealed abstract class MIntLoc extends MIntLocOrValue

  // a location within some stack frame with an int value or a global location
  case class MIntProgLoc(locInfo: RTLocInfo) extends MIntLoc {
    override def toString: String = s"INT[${locInfo.nesting}, ${locInfo.offset}]"
  }

  // immediate int value
  case class MIntImmediateValue(d:Int) extends MIntLocOrValue {
    override def toString: String = s"$d"
  }

  // Temporary MInt location (created by code generation)
  case class TempMIntLoc(nr: Int) extends MIntLoc {
    override def toString: String = s"t_$nr"
  }

  //----------------------------------------------------
  // address values and locations

  // Locations that contain an address
  sealed abstract class MAddressLoc

  // a location in an stack frame with an address value or a global variable with an address value
  case class MAddressProgLoc(locInfo: RTLocInfo) extends MAddressLoc{
    override def toString: String = s"ADDR[${locInfo.nesting}, ${locInfo.offset}]"
  }

  // Temporary location that contains an address (created by code generation)
  case class TempMAddressLoc(nr: Int) extends MAddressLoc {
    override def toString: String = s"a_$nr"
  }


  //----------------------------------------------------
  // Conversions between address and locations

  // dereference the address found at an MAddressLoc: convert an r-value of type MAddress
  // to an l-value of type MInt
  case class DeRef(addrLoc: MAddressLoc) extends MIntLoc {
    override def toString: String = s"*[$addrLoc]"
  }

  // compute the address of a MIntLoc: convert an l-value of type MInt
  // to an r-value of type MAddress
  case class MkRef(mIntLoc: MIntLoc) extends MAddressLoc {
    override def toString: String = s"&[$mIntLoc]"
  }


  // Instructions ------------------------------------------------------------------------------------------------------

  // instruction on the (virtual / hypothetical) target machine
  sealed abstract class IntermediateInstr


  //----------------------------------------------------
  // operations on int values and locations

  // Binary operation: x := y op z on MInts
  // dest location x is taken as l-value,
  // operand1, y and operand2, z if it is a location, are taken as r-value
  case class AssignInstr(
                          dest:     MIntLoc,
                          operand1: Option[MIntLoc],
                          op:       Option[MOp],
                          operand2: Option[MIntLocOrValue]
                        ) extends IntermediateInstr {
    override def toString: String = this match {
      case AssignInstr(d, Some(op1), Some(o), Some(op2)) =>
        s"\t$d = $op1 $o $op2\n"
      case AssignInstr(d, Some(op1), None, None) =>
        s"\t$d = $op1\n"
      case AssignInstr(d, None, None, Some(op2)) =>
        s"\t$d = $op2\n"
      case _ => throw new Exception(s"internal error malformed intermediate instruction $this")
    }
  }

  // facilitate generation of assign instructions
  object AssignInstr {

    def apply(dest: MIntLoc, operand1: MIntLoc, op: MOp, operand2: MIntLocOrValue): AssignInstr =
      AssignInstr(dest, Some(operand1), Some(op), Some(operand2))

    def apply(dest: MIntLoc, operand1: MIntLoc): AssignInstr =
      AssignInstr(dest, Some(operand1), None, None)

    def apply(dest: MIntLoc, operand2: MIntImmediateValue): AssignInstr =
      AssignInstr(dest, None, None, Some(operand2))
  }


  // Operations on MInt values
  sealed abstract class MOp
  case object AddOp extends MOp { override def toString: String = "+" }
  case object SubOp extends MOp { override def toString: String = "-" }
  case object MultOp extends MOp { override def toString: String = "*" }
  case object DivOp extends MOp { override def toString: String = "/" }

  abstract class MRelOp
  case object EqOp extends MRelOp { override def toString: String = "==" }
  case object NeOp extends MRelOp { override def toString: String = "!" }
  case object LsOp extends MRelOp { override def toString: String = "<" }
  case object GtOp extends MRelOp { override def toString: String = ">" }
  case object LeOp extends MRelOp { override def toString: String = "<=" }
  case object GeOp extends MRelOp { override def toString: String = ">=" }


  // Write instruction
  case class WriteInstr(v: MIntLocOrValue) extends IntermediateInstr {
    override def toString: String = s"\tWRITE($v)\n"
  }


  //----------------------------------------------------
  // Operations on address values

  // Assignment of address values
  case class AssignAddrInstr(dest: MAddressLoc, source: MAddressLoc) extends IntermediateInstr {
    override def toString: String = s"\t$dest = $source\n"
  }


  //----------------------------------------------------
  // Control instructions

  // Conditional jump:
  // if (operator1 op operator2) goto jumpTo
  case class IfInstr(operand1: MIntLocOrValue, op: MRelOp, operand2: MIntLocOrValue, jumpTo: String) extends IntermediateInstr {
    override def toString: String = s"\tIF($operand1 $op $operand2) GOTO $jumpTo\n"
  }

  // Unconditional Jump: goto label
  case class JumpInstr(label: String) extends IntermediateInstr {
    override def toString: String = s"\tGOTO $label\n"
  }

  // label: Noop
  case class LabeledInstr(label: String) extends IntermediateInstr {
    override def toString: String = s"$label: NOOP\n"
  }

  // label: Noop for entry into procedure
  // labels of procedures need to handled in a way different to mere code labels
  case class ProcEntryInstr(label: String) extends IntermediateInstr {
    override def toString: String = s"$label: NOOP\n"
  }

  // call instruction
  case class CallInstr(callLabel: String) extends IntermediateInstr {
    override def toString: String = s"\tJUMP $callLabel\n"
  }

  // return instruction
  case object ReturnInstr extends IntermediateInstr {
    override def toString: String = s"\tJUMP RR\n"
  }




  //----------------------------------------------------
  // Operations on the runtime stack

  // push MInt value on stack
  case class PushMIntInstr(t: MIntLocOrValue)  extends IntermediateInstr {
    override def toString: String = s"\tPUSH_INT $t\n"
  }

  // push Address value on stack
  case class PushMAddressInstr(a: MAddressLoc) extends IntermediateInstr {
    override def toString: String = s"\tPUSH_ADDR $a\n"
  }

  // push code address on stack
  case class PushCodeAddrInstr(returnLabel: String) extends IntermediateInstr {
    override def toString: String = s"\tPUSH_CODE_ADDR $returnLabel\n"
  }

  // push frame pointer on stack
  case object PushFPInstr                      extends IntermediateInstr {
    override def toString: String = s"\tPUSH_FP\n"
  }

  // pop MInt value from stack
  case object PopMIntInstr                     extends IntermediateInstr {
    override def toString: String = s"\tPOP_INT\n"
  }

  // pop address value from stack
  case object PopMAddressInstr                 extends IntermediateInstr {
    override def toString: String = s"\tPOP_ADDR\n"
  }

  // pop code address from stack and store it in register RR
  case object PopCodeAddrToRRInstr             extends IntermediateInstr {
    override def toString: String = s"\tRR = POP_CODE_ADDR\n"
  }

  // pop frame pointer from stack to register FP
  case object PopFPInstr                       extends IntermediateInstr {
    override def toString: String = s"\tFP = POP_FP\n"
  }

  // copy SP to FP
  case object StoreSPasFPInstr                 extends IntermediateInstr {
    override def toString: String = s"\tFP = SP\n"
  }

  //----------------------------------------------------
  // Instructions that allocate static data areas

  // allocate static storage with size storage cells
  case class AllocStaticInstr(size: Int)  extends IntermediateInstr {
    override def toString: String = s"\tALLOC(size: $size)\n"
  }



  // -------------------------------------------------------------------------------------------------------------------

  private val tempsCount = 25 // number of temporary locations (registers) for MInt and MAddress each



  /**
    * Instances of this class take a code builder,
    * create a code generator that uses this code builder, and generate target code using it.
    *
    * @param codeBuilder the code builder that contains the machine dependent details od transforming
    *                    intermediate code to machine code.
    */
  case class With(codeBuilder: CodeBuilder) {
    def genCodeFor(obj: Obj): String = {
      val actualCodeGenerator = new CodeGenerator(codeBuilder)
      actualCodeGenerator.translate(obj)
      codeBuilder.getCode
    }
  }

}


/**
  * Instances know how to transform an AST to intermediate code.
  * They generate code by producing intermediate code and then passing it to a code builder which
  * transforms intermediate to target code.
  *
  * @param codeBuilder the code builder used generate real code from intermediate code.
  *
  */
class CodeGenerator(codeBuilder: CodeBuilder) {

  import CodeGenerator._


  // -------------------------------------------------------------------------------------------------------------------
  // Definition of intermediate code

  // create temporary locations
  private var tempsMInt: List[Int] = (0 to tempsCount).toList

  // get next unused MInt temporary
  private def acquireMIntTemp(): TempMIntLoc = {
    val res = TempMIntLoc(tempsMInt.head)
    tempsMInt = tempsMInt.tail
    res
  }

  // release a MInt temporary
  private def releaseMIntTemp(t: TempMIntLoc) : Unit = {
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
  // codeBuilder is used to actually create code


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
      codeBuilder += AssignInstr(target, t1, op, t2)
      releaseMIntTemp(t1)
      releaseMIntTemp(t2)
    }

    exp match {
      case Add(l, r) => genBinOp(l, AddOp, r, target)
      case Sub(l, r) => genBinOp(l, SubOp, r, target)
      case Mul(l, r) => genBinOp(l, MultOp, r, target)
      case Div(l, r) => genBinOp(l, DivOp, r, target)
      case LocAccess(locExp) =>
        val t = genCodeIntLocExp(locExp) // put location denoted by locExp into t
        if (t != target) { // avoid assignment to itself
          codeBuilder += AssignInstr(target, t)
        }
      case n@Number(_) =>
        codeBuilder += AssignInstr(target, MIntImmediateValue(n.d))
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
      codeBuilder += IfInstr(t1, compOp, t2, trueLabel)
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
      codeBuilder += JumpInstr(exitLabel)
      codeBuilder += LabeledInstr(thenLabel)
      thenCmds.foreach { cmd => genCodeCmd(cmd) }
      codeBuilder += LabeledInstr(exitLabel)

    case If(cond, thenCmds, elseCmds) =>
      val thenLabel = newLabel
      val exitLabel = newLabel
      genCodeBe(cond, thenLabel)
      elseCmds.foreach { cmd => genCodeCmd(cmd) }
      codeBuilder += JumpInstr(exitLabel)
      codeBuilder += LabeledInstr(thenLabel)
      thenCmds.foreach { cmd => genCodeCmd(cmd) }
      codeBuilder += LabeledInstr(exitLabel)

    case While(cond, body) =>
      val startLabel = newLabel
      val continueLabel = newLabel
      val endLabel = newLabel
      codeBuilder += LabeledInstr(startLabel)
      genCodeBe(cond, continueLabel)
      codeBuilder += JumpInstr(endLabel)
      codeBuilder += LabeledInstr(continueLabel)
      body.foreach { cmd => genCodeCmd(cmd) }
      codeBuilder += JumpInstr(startLabel)
      codeBuilder += LabeledInstr(endLabel)

    case Call(symb, args) => symb match {
      case pSymb@ProcSymbol(_, Some(_), Some(_)) =>

        codeBuilder += PushFPInstr   // push actual frame pointer

        // push arguments
        args.reverse.foreach {  // put arguments on stack
          case Arg(rExp, Some(ByValue)) => // pass by value
            val t = acquireMIntTemp()
            genCodeValExp(rExp, t)
            codeBuilder += PushMIntInstr(t)
            releaseMIntTemp(t)
          case Arg(lExp, Some(ByRef)) => // pass by reference
            lExp match {
              case LocAccess(locExp) =>
                val intLoc = genCodeIntLocExp(locExp)
                val a = acquireMAddressTemp()
                codeBuilder += AssignAddrInstr(a, MkRef(intLoc))
                codeBuilder += PushMAddressInstr(a)
                releaseMAddressTemp(a)
              case _ => throw new Exception(s"internal error: illegal passing of value argument by ref")
            }
          case arg@Arg(_, None) => throw new Exception(s"internal error: malformed argument $arg")
        }

        val returnLabel = newLabel // label behind call instruction

        // push return address
        codeBuilder += PushCodeAddrInstr(returnLabel)

        codeBuilder += CallInstr(procSymbToLabel(pSymb))  // call procedure
        codeBuilder += LabeledInstr(returnLabel)

        // clean stack in reverse order
        // return address has already been poped by called procedure
        args.foreach { // pop arguments
          case Arg(_, Some(ByValue)) =>
            codeBuilder += PopMIntInstr
          case Arg(_, Some(ByRef)) =>
            codeBuilder += PopMAddressInstr
          case arg@Arg(_, None) => throw new Exception(s"internal error: malformed argument $arg")
        }

        // restore frame pointer
        codeBuilder += PopFPInstr

      case _ => throw new Exception(s"Procedure expected but ${symb.name} found")
    }

    case Write(e) =>
      val t = acquireMIntTemp()
      genCodeValExp(e, t)
      codeBuilder += WriteInstr(t)

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
      codeBuilder += AssignAddrInstr(target, MAddressProgLoc(locSymb.rtLocInfo.get))
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
    codeBuilder += ProcEntryInstr(procLabel)

    // store SP as new FP
    codeBuilder += StoreSPasFPInstr

    // allocate non-static locals on stack
    procDef.locals.foreach {
      case VarDef(symb, _, _) =>
        if (symb.rtLocInfo.get.nesting > 0 ) { // deal with stack variables only
          symb.staticType match {
            case Some(IntTypeInfo) =>
              codeBuilder += PushMIntInstr(MIntImmediateValue(0))
            case _ => throw new Exception("internal error language supports only int variables")
          }
        }
    }

    // fill locals with init values
    procDef.locals.foreach {
      case VarDef(symb, _, initExp) =>
        val varLoc = MIntProgLoc(symb.rtLocInfo.get)
        genCodeValExp(initExp, varLoc)
      case _ => // ignore
    }

    // code for body
    procDef.cmds.foreach( cmd => genCodeCmd(cmd) )

    // release locals on stack
    procDef.locals.reverse.foreach {
      case VarDef(symb, _, _) =>
        if (symb.rtLocInfo.get.nesting > 0 ) { // deal with stack variables only
          symb.staticType match {
            case Some(IntTypeInfo) =>
              codeBuilder += PopMIntInstr
            case _ => throw new Exception("internal error language supports only int variables")
          }
        }
    }

    // set RR from stack
    codeBuilder += PopCodeAddrToRRInstr

    // return to caller
    codeBuilder += ReturnInstr

  }


  /*
   * Generate code for a global variable.
   * @param varDef the definition of the global variable.
   */
  private def genCodeGlobVarDef(varDef: VarDef): Unit = varDef match {
    case VarDef(varSymb, _, _) =>
      varSymb.staticType match {
        case Some(IntTypeInfo) =>
          codeBuilder += AllocStaticInstr(RuntimeOrganisation.addressableCellsPerInt)
        case _ => throw new Exception(s"internal error: symbol ${varSymb.name} should be of type int")
      }
  }


  /*
   * Generate code for initializations of all global variable defintions and
   * code for the initialization section of an object.
   * The code ist created as code of a procedure. (A virtual or pseudo procedure created by the compiler)
   * @param obj the object for which the initializing pseudo procedure is to be generated.
   */
  private def genInitProc(obj: Obj): Unit = {
    // create pseudo proc
    val pseudoProc = ProcDef(
      ProcSymbol(
        "OBJ_INIT_PROC___", // name
        Some(Nil),          // parameters
        Some(Nil)),         // locals
      Nil,                  // parameters
      obj.defs.flatMap{     // locals
        case v:VarDef => List(v)
        case _ => Nil
      },
      obj.cmds
    )
    genCodeProc(pseudoProc)
  }

  /**
    * Translate an object to code using the codeBuilder.
    * @param obj the object to be translated
    * @return    code for obj
    */
  private[backend] def translate(obj: Obj): String = {

    // pass the name of the compilation unit to the code generator
    codeBuilder.direct(codeBuilder.CuNameDirective(obj.name))

    // pass the imports to the code generator
    obj.imports.foreach {
      case (objName, symb) => codeBuilder.direct(
        codeBuilder.ImportDirective(objName, symb.name))
    }

    // pass the exports to the code generator
    obj.exports.foreach {
      case defName => codeBuilder.direct(
        codeBuilder.ExportDirective(defName))
    }

    // translate definition of object (global) variables
    obj.defs foreach {
      case varDef@VarDef(_, _, _) =>
        genCodeGlobVarDef(varDef)
      case _ =>
    }

    // translate initialisation of global variables
    // and init section to init procedure
    genInitProc(obj)


    // translate procedure definitions
    obj.defs foreach {
      case procDef@ProcDef(_, _, _, _) =>
        genCodeProc(procDef)
      case _ =>
    }

    codeBuilder.toString

  }

}

