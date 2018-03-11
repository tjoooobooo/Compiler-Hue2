package ZwischenCode

import backend.RuntimeOrganisation.RTLocInfo

  /**
    * Companion object of the code generator class.
    * Contains
    *  - the definition of the intermediate code and its components.
    *  - the code builder class wich actually generates code.
    */
  object ZwischenCodeAST {

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
    case object ModOp extends MOp { override def toString: String = "%" }
    //bitwise operators
    case object AndOp extends MOp { override def toString: String = "&" }
    case object OrOp extends MOp  { override def toString: String = "|" }
    case object XorOp extends MOp { override def toString: String = "^" }
    case object SlOp extends MOp  { override def toString: String = "<<" }
    case object SrOp extends MOp  { override def toString: String = ">>" }

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
    case class ReadInstr(v: MIntLocOrValue) extends IntermediateInstr {
      override def toString: String = s"\tREAD($v)\n"
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





}
