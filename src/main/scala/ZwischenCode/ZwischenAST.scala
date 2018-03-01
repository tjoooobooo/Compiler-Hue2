package ZwischenCode

import CodeGenerator.Address
import backend.RuntimeOrganisation.RTLocInfo

object ZwischenAST {

  object AssignInstr {
    def apply(dest: MIntLoc, operand1: MIntLoc, op: MOp, operand2: MIntLocOrValue): AssignInstr =
      AssignInstr(dest, Some(operand1), Some(op), Some(operand2))
    def apply(dest: MIntLoc, operand1: MIntLoc): AssignInstr =
      AssignInstr(dest, Some(operand1), None, None)
    def apply(dest: Location, operand2: MIntImmediateValue): AssignInstr =
      AssignInstr(dest, None, None, Some(operand2))
  }

  sealed abstract class MIntLocOrValue extends Location
  // a location with an int value
  sealed abstract class MIntLoc extends MIntLocOrValue
  // a location in an stack frame with an int value
  case class MIntFrameLoc(locInfo: RTLocInfo) extends MIntLoc
  // immediate int value
  case class MIntImmediateValue(d:Int) extends MIntLocOrValue
  // Temporary MInt location (created by code generation)
  case class TempMIntLoc(nr: Int) extends MIntLoc
  // address values and locations
  // Locations that contain an address
  sealed abstract class MAddressLoc
  // a location in an stack frame with an address value
  case class MAddressFrameLoc(locInfo: RTLocInfo) extends MAddressLoc
  // Temporary location that contains an address (created by code generation)
  case class TempMAddressLoc(nr: Int) extends MAddressLoc

  sealed abstract class LocOrValue
  // Location
  sealed abstract class Location extends LocOrValue
  // Numeric value
  case class ImmediateValue(x: Int) extends LocOrValue
  // Temporary location (created by code generation)
  case class TempLoc(nr: Int) extends MIntLocOrValue
  // Operations on the (virtual) target machine
  sealed abstract class MOp
  case object AddOp extends MOp
  case object SubOp extends MOp
  case object MultOp extends MOp
  case object DivOp extends MOp
  // instruction on the (virtual) target machine
  sealed abstract class Instr
  // Binary operation: x := y op z
  case class AssignInstr(
                          dest: Location,
                          operand1: Option[Location],
                          op: Option[MOp],
                          operand2: Option[LocOrValue]
                        ) extends Instr

  // Conditional jump
  // if (operator1 op operator2) goto jumpTo
  case class IfInstr(
                      operand1: MIntLocOrValue,
                      op: MRelOp,
                      operand2: MIntLocOrValue,
                      jumpTo: String
                    ) extends Instr
  // Unconditional Jump: goto label
  case class JumpInstr(label: String) extends Instr
  // label: Noop
  case class LabeledInstr(label: String) extends Instr
  // Compare operations
  abstract class MRelOp
  case object EqOp extends MRelOp
  case object NeOp extends MRelOp
  case object LsOp extends MRelOp
  case object GtOp extends MRelOp
  case object LeOp extends MRelOp
  case object GeOp extends MRelOp

}
