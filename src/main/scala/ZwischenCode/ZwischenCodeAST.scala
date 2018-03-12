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
    case class MIntProgLoc(locInfo: RTLocInfo) extends MIntLoc

    // immediate int value
    case class MIntImmediateValue(d:Int) extends MIntLocOrValue

    // Temporary MInt location (created by code generation)
    case class TempMIntLoc(nr: Int) extends MIntLoc

    //----------------------------------------------------
    // address values and locations

    // Locations that contain an address
    sealed abstract class MAddressLoc

    // a location in an stack frame with an address value or a global variable with an address value
    case class MAddressProgLoc(locInfo: RTLocInfo) extends MAddressLoc

    // Temporary location that contains an address (created by code generation)
    case class TempMAddressLoc(nr: Int) extends MAddressLoc


    //----------------------------------------------------
    // Conversions between address and locations

    // dereference the address found at an MAddressLoc: convert an r-value of type MAddress
    // to an l-value of type MInt
    case class DeRef(addrLoc: MAddressLoc) extends MIntLoc

    // compute the address of a MIntLoc: convert an l-value of type MInt
    // to an r-value of type MAddress
    case class MkRef(mIntLoc: MIntLoc) extends MAddressLoc


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
                          ) extends IntermediateInstr


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
    case object AddOp extends MOp
    case object SubOp extends MOp
    case object MultOp extends MOp
    case object DivOp extends MOp
    case object ModOp extends MOp
    //bitwise operators
    case object AndOp extends MOp
    case object OrOp extends MOp
    case object XorOp extends MOp
    case object SlOp extends MOp
    case object SrOp extends MOp

    abstract class MRelOp
    case object EqOp extends MRelOp
    case object NeOp extends MRelOp
    case object LsOp extends MRelOp
    case object GtOp extends MRelOp
    case object LeOp extends MRelOp
    case object GeOp extends MRelOp


    // Write instruction
    case class WriteInstr(v: MIntLocOrValue) extends IntermediateInstr

    case class ReadInstr(v: MIntLocOrValue) extends IntermediateInstr



    //----------------------------------------------------
    // Operations on address values

    // Assignment of address values
    case class AssignAddrInstr(dest: MAddressLoc, source: MAddressLoc) extends IntermediateInstr


    //----------------------------------------------------
    // Control instructions

    // Conditional jump:
    // if (operator1 op operator2) goto jumpTo
    case class IfInstr(operand1: MIntLocOrValue, op: MRelOp, operand2: MIntLocOrValue, jumpTo: String) extends IntermediateInstr

    // Unconditional Jump: goto label
    case class JumpInstr(label: String) extends IntermediateInstr

    // label: Noop
    case class LabeledInstr(label: String) extends IntermediateInstr

    // label: Noop for entry into procedure
    // labels of procedures need to handled in a way different to mere code labels
    case class ProcEntryInstr(label: String) extends IntermediateInstr

    // call instruction
    case class CallInstr(callLabel: String) extends IntermediateInstr

    // return instruction
    case object ReturnInstr extends IntermediateInstr




    //----------------------------------------------------
    // Operations on the runtime stack

    // push MInt value on stack
    case class PushMIntInstr(t: MIntLocOrValue)  extends IntermediateInstr

    // push Address value on stack
    case class PushMAddressInstr(a: MAddressLoc) extends IntermediateInstr

    // push code address on stack
    case class PushCodeAddrInstr(returnLabel: String) extends IntermediateInstr

    // push frame pointer on stack
    case object PushFPInstr                      extends IntermediateInstr

    // pop MInt value from stack
    case object PopMIntInstr                     extends IntermediateInstr

    // pop address value from stack
    case object PopMAddressInstr                 extends IntermediateInstr

    // pop code address from stack and store it in register RR
    case object PopCodeAddrToRRInstr             extends IntermediateInstr

    // pop frame pointer from stack to register FP
    case object PopFPInstr                       extends IntermediateInstr


    // copy SP to FP
    case object StoreSPasFPInstr                 extends IntermediateInstr

    //----------------------------------------------------
    // Instructions that allocate static data areas

    // allocate static storage with size storage cells
    case class AllocStaticInstr(size: Int)  extends IntermediateInstr



    // -------------------------------------------------------------------------------------------------------------------

}
