package CodeGenerator

import backend.RuntimeOrganisation.RTLocInfo
import frontend.AST._

import scala.collection.mutable.ListBuffer

object TestCode {


  object AssignInstr {


    def apply(dest: MIntLoc, operand1: MIntLoc, op: MOp, operand2: MIntLocOrValue): AssignInstr =
      AssignInstr(dest, Some(operand1), Some(op), Some(operand2))
    def apply(dest: MIntLoc, operand1: MIntLoc): AssignInstr =
      AssignInstr(dest, Some(operand1), None, None)
    def apply(dest: MIntLoc, operand2: MIntImmediateValue): AssignInstr =
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
    case class TempLoc(nr: Int) extends Location
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

  /*private def genCodeCmd(cmd: Cmd): Unit = cmd match {
    case Assign(left, right) =>
      val target = genCodeIntLocExp(left) // target location now contains the the destination loc
      genCodeValExp(right, target) // generate code that puts value of right to target
      ...
  }*/


  def genCode(exp: Exp) : List[Instr] = {
    // create temporary locations
    var temps: List[Int] = (0 to tempsCount).toList
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
    def genCodeValExp(exp: Exp, target: Location): Unit = exp match {
      case Add(l, r) => genBinOp(l, AddOp, r, target)
      case Sub(l, r) => genBinOp(l, SubOp, r, target)
      case Mul(l, r) => genBinOp(l, MultOp, r, target)
      case Div(l, r) => genBinOp(l, DivOp, r, target)
      case Number(v) =>
        codeBuf += AssignInstr(target, Option(MIntImmediateValue(v)))
    }
    var t = acquireTemp()
    genCodeExp(exp, t)
    codeBuf.toList
  }

}
