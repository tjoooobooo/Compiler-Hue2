package CodeGenerator

import CodeGenerator.AssemblerAST._
import ZwischenCode.ZwischenCodeAST._
import ZwischenCode.ZwischenCodeGenerator._

import scala.collection.mutable.ListBuffer
object GenAssemblerLines {

  def gen(zwischenCode: List[IntermediateInstr]): List[AssemblerLine] = {
    var listBuilder : ListBuffer[AssemblerLine] = new ListBuffer[AssemblerLine]
    zwischenCode foreach println
    println("-----------------------------------")
    var hasGlobalVars: Boolean = false
    zwischenCode foreach {
      case AssignInstr(dest, operand1, op, operand2) =>

        var op1: Int = operand1 match {
          case Some(MIntProgLoc(locInfo)) => locInfo.nesting
          case Some(TempMIntLoc(nr)) => nr
          case _ => 0
        }

        var op2: Int = operand2 match {
          case Some(MIntImmediateValue(d)) => d
          case Some(MIntProgLoc(locInfo)) => println(locInfo)
            0
          case Some(TempMIntLoc(nr)) => nr
            nr
          case _ => 0
        }

        dest match {
          case MIntProgLoc(locInfo) =>
            if (locInfo.nesting == 0) {
              if(operand1.isDefined){
                solveAssignExp(operand1, op, operand2)
                listBuilder += Setw(getValue(operand2), Right("global_vars"))
                listBuilder += Stw(getValue(operand1),getValue(operand2),0)
              } else {
                var t,t2 = acquireMIntTemp()
                listBuilder += Setw(t.nr,Left(getValue(operand2)))
                listBuilder += Setw(t2.nr, Right("global_vars"))
                listBuilder += Stw(t.nr,t2.nr,-locInfo.offset*4)
                releaseMIntTemp(t)
              }

              // TODO vielleicht muss ein Addc(1,1,4) hier hin
            }
          case TempMIntLoc(nr) =>
            operand1 match {
              case Some(MIntProgLoc(locInfo)) =>
                listBuilder += Setw(nr,Right("global_vars"))
                listBuilder += Stw(nr,nr,-locInfo.offset*4)
              case None => listBuilder += Setw(nr, Left(getValue(operand2)))
            }

          case DeRef(addrLoc) => listBuilder += Ldw(getRegisterDeRef(addrLoc), getRegisterDeRef(addrLoc), 0)
          case _ => println("ASSIGN MATCH ERROR--------------------------")
        }

      case WriteInstr(v) =>
        v match {
          case TempMIntLoc(nr) => listBuilder += Outi(nr)
          case MIntProgLoc(locInfo) =>
        }

      case ReadInstr(v) => println("ReadInstr " + v)

      case AssignAddrInstr(dest, source) => println("AssignAddrInstr " + dest + " " + source)
        var nesting: Int = 0
        var offset: Int = 0
        source match {
          case rtloc: MAddressProgLoc =>
            nesting = rtloc.locInfo.nesting
            offset = rtloc.locInfo.offset
          case MkRef(mIntLoc) =>
        }
        dest match {
          case loc: TempMAddressLoc =>
            if (nesting.equals(1)) listBuilder += Addc(loc.nr, 29, 1 + (offset - 2) * 4)
          case _ =>
        }
        listBuilder += Ldw(getRegisterDeRef(dest), getRegisterDeRef(dest), 0)

      case IfInstr(operand1, op, operand2, jumpTo) =>

      case JumpInstr(label) =>

      case LabeledInstr(label) =>

      case ProcEntryInstr(label) => listBuilder += Label(label)

      case CallInstr(callLabel) =>

      case ReturnInstr => listBuilder += Jmpr(30)

      case PushMIntInstr(t) =>

      case PushMAddressInstr(a) =>

      case PushCodeAddrInstr(returnLabel) =>

      case PushFPInstr =>

      case PopMIntInstr =>

      case PopMAddressInstr => listBuilder += Addc(31, 31, 4)

      case PopCodeAddrToRRInstr =>

      case PopFPInstr =>

      case StoreSPasFPInstr => listBuilder += Addc(29, 31, 0)

      case AllocStaticInstr(size) =>
        if (!hasGlobalVars) listBuilder += Label("global_vars")
        hasGlobalVars = true
        listBuilder += WordDirective(None)

      case _ => println("INSTRUKTION ERROR-----------------------")
    }

    def getRegisterDeRef(addrLoc: MAddressLoc): Int = addrLoc match {
      case TempMAddressLoc(nr) => nr
      case MAddressProgLoc(locInfo) => 0
    }

    def getValue(value: Option[MIntLocOrValue]): Int = value.get match {
      case MIntImmediateValue(d) => d
      case TempMIntLoc(nr) => nr
    }

    def tempToReg(temp: Option[MIntLoc]) : Int = temp.get match {
      case TempMIntLoc(nr) => nr
    }

    def solveAssignExp(maybeLoc: Option[MIntLoc], maybeOp: Option[MOp], maybeValue: Option[MIntLocOrValue]): Unit = {

        listBuilder += (maybeOp match {
          case Some(AddOp) => Add(tempToReg(maybeLoc), tempToReg(maybeLoc), getValue(maybeValue))
          case Some(SubOp) => Sub(tempToReg(maybeLoc), tempToReg(maybeLoc), getValue(maybeValue))
          case Some(MultOp) => Muli(tempToReg(maybeLoc), tempToReg(maybeLoc), getValue(maybeValue))
          case Some(DivOp) => Divi(tempToReg(maybeLoc), tempToReg(maybeLoc), getValue(maybeValue))
          case Some(ModOp) => Modi(tempToReg(maybeLoc), tempToReg(maybeLoc), getValue(maybeValue))
          case Some(AndOp) => And(tempToReg(maybeLoc), tempToReg(maybeLoc), getValue(maybeValue))
          case Some(OrOp) => Or(tempToReg(maybeLoc), tempToReg(maybeLoc), getValue(maybeValue))
          case Some(XorOp) => Xor(tempToReg(maybeLoc), tempToReg(maybeLoc), getValue(maybeValue))
          case Some(SlOp) => Sl(tempToReg(maybeLoc), tempToReg(maybeLoc), getValue(maybeValue))
          case Some(SrOp) => Sr(tempToReg(maybeLoc), tempToReg(maybeLoc), getValue(maybeValue))
        })

    }
    listBuilder += Jmpr(30)
    listBuilder.toList
  }

}

