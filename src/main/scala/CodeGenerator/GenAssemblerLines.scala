package CodeGenerator

import CodeGenerator.AssemblerAST._
import ZwischenCode.ZwischenCodeAST._
object GenAssemblerLines {

  def gen(zwischenCode: List[IntermediateInstr]): Unit ={
    println(zwischenCode)
    println("------------------------------")
    zwischenCode foreach {
      case AssignInstr(dest,operand1,op,operand2) =>

        println("ASSIGNINSTRUKTION")
        dest match {
          case MIntProgLoc(locInfo) => println("MINTPROG " + locInfo + " " + operand2+ " " + operand1)
          case TempMIntLoc(nr)      => println("TEMPLOC " + nr+ " " + operand2+ " " + operand1)
          case DeRef(addrLoc)       => println("DEREF " + addrLoc+ " " + operand2+ " " + operand1)
          case _ => println("ASSIGN MATCH ERROR")
        }
        /*
        op match {
          case Some(AddOp)  => Add()
          case Some(SubOp)  => Sub()
          case Some(MultOp) => Muli()
          case Some(DivOp)  => Divi()
          case Some(ModOp)  => Modi()
          case Some(AndOp)  => And()
          case Some(OrOp)   => Or()
          case Some(XorOp)  => Xor()
          case Some(SlOp)   => Sl()
          case Some(SrOp)   => Sr()
          case _ =>
        }
      */
      case WriteInstr(v) =>
        v match {
          case TempMIntLoc(nr) => Outi(nr)
          case MIntProgLoc(locInfo) =>
        }

      case ReadInstr(v) => println("ReadInstr "+v)

      case AssignAddrInstr(dest,source) => println("AssignAddrInstr " + dest +" "+source)

      case IfInstr(operand1,op,operand2,jumpTo) =>

      case JumpInstr(label) => println("JUMPINSTR " + Jmp(label))

      case LabeledInstr(label) => println("labeled instr" + label)

      case ProcEntryInstr(label) => println(Label(label))

      case CallInstr(callLabel) => println("CALL " + Call(30,callLabel))

      case ReturnInstr => println("RETURNINSTR ")

      case PushMIntInstr(t) => println("PUSHMINT " + t)

      case PushMAddressInstr(a) => println("PushMAddressInstr " + a)

      case PushCodeAddrInstr(returnLabel) => println("PushCodeAddrInstr " + returnLabel)

      case PushFPInstr => println("PushFPInstr")

      case PopMIntInstr =>println("PopMIntInstr")

      case PopMAddressInstr =>println("PopMAddressInstr")

      case PopCodeAddrToRRInstr =>println("PopCodeAddrToRRInstr")

      case PopFPInstr =>println("PopFPInstr")

      case StoreSPasFPInstr => println(Addc(29,31,0))

      case AllocStaticInstr(size) =>println("AllocStaticInstr "+size)

      case _ =>
    }
  }

}

