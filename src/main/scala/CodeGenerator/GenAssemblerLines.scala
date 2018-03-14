package CodeGenerator

import CodeGenerator.AssemblerAST._
import ZwischenCode.ZwischenCodeAST._
import ZwischenCode.ZwischenCodeGenerator._
import backend.RuntimeOrganisation.RTLocInfo

import scala.collection.mutable.ListBuffer
object GenAssemblerLines {

  def gen(zwischenCode: List[IntermediateInstr]): List[AssemblerLine] = {
    var listBuilder: ListBuffer[AssemblerLine] = new ListBuffer[AssemblerLine]
    var procOffset : Int = 0
    var procCounter : Option[Int] = None

    listBuilder += ObjectDirective("Test")
    listBuilder += ExecutableDirective("main")
    listBuilder += ExportDirective("main")

    zwischenCode foreach println
    println("-----------------------------------")

  var paramCounter = 0
  for(code <- zwischenCode) code match {
    case PushMIntInstr(_)|
    PushMAddressInstr(_) => paramCounter += 1
    case _ =>
  }

    var hasGlobalVars: Boolean = false
    for(code <- zwischenCode) code match {

      case AssignInstr(dest, operand1, op, operand2) =>

        dest match {
          case MIntProgLoc(locInfo) =>
            // GLOBALE VARIABLEN zuweisungen
            if (locInfo.nesting == 0) {
              if (operand1.isDefined & operand2.isDefined) {
                solveAssignExp(operand1, op, operand2)
                listBuilder += Setw(getValue(operand2), Right("global_vars"))
                listBuilder += Stw(getValue(operand1), getValue(operand2), -locInfo.offset * 4)
              } else if(operand2.isDefined) {
                // AssignInstr(MIntProgLoc(RTLocInfo(0,0)),None,None,Some(MIntImmediateValue(5)))
                var t, t2 = acquireMIntTemp()
                listBuilder += Setw(t.nr, Left(getValue(operand2)))
                listBuilder += Setw(t2.nr, Right("global_vars"))
                listBuilder += Stw(t.nr, t2.nr, -locInfo.offset * 4)
                releaseMIntTemp(t)
                releaseMIntTemp(t2)
              } else if(operand1.isDefined) {
                // AssignInstr(MIntProgLoc(RTLocInfo(0,0)),Some(MIntProgLoc(RTLocInfo(0,-1))),None,None)
                operand1.get match {
                  case MIntProgLoc(info) =>
                    var t,t2 = acquireMIntTemp()
                    listBuilder += Setw(t.nr, Right("global_vars"))
                    listBuilder += Ldw(t.nr,t.nr,-locInfo.offset * 4)
                    listBuilder += Setw(t2.nr, Right("global_vars"))
                    listBuilder += Stw(t.nr, t2.nr, -info.offset * 4)
                    releaseMIntTemp(t)
                    releaseMIntTemp(t2)
                  // t steht wert drinnen
                }
              }
              // lokale Variablen
            } else {
              var t = acquireMIntTemp()
              var t2 = acquireMIntTemp()
              listBuilder += Addc(t.nr,29,1 + (locInfo.offset - 2) * 4)
              if(operand1.isDefined & op.isEmpty) {
                operand1.get match {
                  case MIntProgLoc(info) =>
                    solveProgLoc(info,t2)
                }
                listBuilder += Stw(t2.nr,t.nr,0)
              } else if(operand2.isDefined & op.isEmpty){
                operand2.get match {
                  case MIntImmediateValue(d) => listBuilder += Setw(t2.nr,Left(d))
                }
                listBuilder += Stw(t.nr, t2.nr, 0)
              } else {
                solveAssignExp(operand1, op, operand2)
                listBuilder += Stw(t.nr, getValue(operand1), 0)
              }
              releaseMIntTemp(t)
              releaseMIntTemp(t2)

            }
          case TempMIntLoc(nr) =>
            operand1 match {
              case Some(MIntProgLoc(locInfo)) =>
                if(locInfo.nesting == 0){
                  listBuilder += Setw(nr, Right("global_vars"))
                  listBuilder += Ldw(nr, nr, -locInfo.offset * 4)
                } else {
                  solveProgLoc(locInfo,TempMIntLoc(nr))
                }


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

      case AssignAddrInstr(dest, source) =>
        var nesting: Int = 0
        var offset: Int = 0
        source match {
          case rtloc: MAddressProgLoc =>
            nesting = rtloc.locInfo.nesting
            offset = rtloc.locInfo.offset
          case MkRef(mIntLoc) =>
           mIntLoc match {
             case MIntProgLoc(info) =>
               dest match{
                 case TempMAddressLoc(nr) =>
                   if(procCounter.isEmpty) procCounter = Some(-info.offset)
                   listBuilder += Setw(nr,Right("global_vars"))
                   listBuilder += Addc(nr,nr,(procCounter.get + info.offset)*4)
                   listBuilder += Ldw(nr,nr,0)
               }
           }


        }
        dest match {
          case loc: TempMAddressLoc =>
            if (nesting.equals(1)) {
              listBuilder += Addc(loc.nr, 29, 1 + (offset - 2) * 4)
              listBuilder += Ldw(getRegisterDeRef(dest), getRegisterDeRef(dest), 0)
            }
            else {
              //listBuilder +=  Setw(loc.nr,)
            }
          case _ =>
        }


      case IfInstr(operand1, op, operand2, jumpTo) =>
        solveBoolExp(operand1, op, operand2)
        listBuilder += Brt(getValue(Option(operand1)), jumpTo)


      case JumpInstr(label) => listBuilder += Jmp(label)

      case LabeledInstr(label) => listBuilder += Label(label)

      case ProcEntryInstr(label) =>
        listBuilder += Label(label)
        //countParams(code)
        //zwischenCode.for()


      case CallInstr(callLabel) =>
        listBuilder += Call(30, callLabel)
        paramCounter = 0

      case ReturnInstr => listBuilder += Jmpr(30)

      case PushMIntInstr(t) =>
        t match{
          case TempMIntLoc(nr) =>
            listBuilder += Setw(nr+1,Left(procOffset*4+1))
            listBuilder += Add(nr+1,nr+1,31)
            listBuilder += Stw(nr,nr+1,0)
            procOffset += 1
        }



      case PushMAddressInstr(a) =>
        a match{
          case TempMAddressLoc(nr) =>
            listBuilder += Setw(nr+1,Left(procOffset*4+1))
            listBuilder += Add(nr+1,nr+1,31)
            listBuilder += Stw(nr,nr+1,0)
            procOffset += 1
        }

      case PushCodeAddrInstr(returnLabel) =>

      case PushFPInstr =>
        listBuilder += Subc(31,31,8)
        listBuilder += Stw(29,31,1)
        listBuilder += Stw(30,31,5)
        //TODO parameter zählen?


      case PopMIntInstr =>

      case PopMAddressInstr => //listBuilder += Addc(31, 31, 4)

      case PopCodeAddrToRRInstr =>
      listBuilder += Addc(31,31,paramCounter*4)

      case PopFPInstr =>
        //listBuilder += Subc(31,31,procOffset*4)
        listBuilder += Ldw(30, 31, 5)
        listBuilder += Ldw(29, 31, 1)
        listBuilder += Addc(31,31,8)

      case StoreSPasFPInstr => listBuilder += Addc(29, 31, 0)

      case AllocStaticInstr(size) =>
        if (!hasGlobalVars) listBuilder += Label("global_vars")
        hasGlobalVars = true
        listBuilder += WordDirective(None)

      case _ => println("INSTRUKTION ERROR-----------------------")
    }

    def countParams(instr: IntermediateInstr): Unit ={
      var counter = zwischenCode.indexOf(instr) + 1
      while(counter < zwischenCode.length){
        zwischenCode(counter) match {
          case PushMAddressInstr(_) | PushMIntInstr(_) => procOffset += 1
          case CallInstr(_) => return
          case _=>
        }
        counter += 1
      }
    }

    def getRegisterDeRef(addrLoc: MAddressLoc): Int = addrLoc match {
      case TempMAddressLoc(nr) => nr
      case MAddressProgLoc(locInfo) => 0
    }

    def getValue(value: Option[MIntLocOrValue]): Int = value.get match {
      case MIntImmediateValue(d) => d
      case TempMIntLoc(nr) => nr
    }

    def tempToReg(temp: Option[MIntLoc]): Int = temp.get match {
      case TempMIntLoc(nr) => nr
    }

    def solveAssignExp(maybeLoc: Option[MIntLoc], maybeOp: Option[MOp], maybeValue: Option[MIntLocOrValue]): Unit = {
      if (maybeOp.isDefined) {
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
    }

      def solveProgLoc(info: RTLocInfo, t: TempMIntLoc): Unit = {
          listBuilder += Addc(t.nr,29,1 + (info.offset - 2) * 4)
          listBuilder += Ldw(t.nr,t.nr,0)

      }

      def solveBoolExp(value: MIntLocOrValue, op: MRelOp, value2: MIntLocOrValue): Unit = {
        listBuilder += (op match {
          case EqOp => Eq(getValue(Option(value)), getValue(Option(value)), getValue(Option(value2)))
          case NeOp => Ne(getValue(Option(value)), getValue(Option(value)), getValue(Option(value2)))
          case LsOp => Lti(getValue(Option(value)), getValue(Option(value)), getValue(Option(value2)))
          case LeOp => Lei(getValue(Option(value)), getValue(Option(value)), getValue(Option(value2)))
          case GeOp => Gti(getValue(Option(value)), getValue(Option(value)), getValue(Option(value2)))
          case GtOp => Gei(getValue(Option(value)), getValue(Option(value)), getValue(Option(value2)))
        })
      }

      listBuilder += Jmpr(30)
      listBuilder.toList
    }
}

