package CodeGenerator

import CodeGenerator.AssemblerAST._
import ZwischenCode.ZwischenCodeAST._
import ZwischenCode.ZwischenCodeGenerator._
import backend.RuntimeOrganisation.RTLocInfo

import scala.collection.mutable.ListBuffer
object GenAssemblerLines {

  def gen(zwischenCode: List[IntermediateInstr]): List[AssemblerLine] = {
    var listBuilder: ListBuffer[AssemblerLine] = new ListBuffer[AssemblerLine]
    var procTillCall : Int = 0
    var functionparameters: Int = 0
    var tempMAddressLoc: Int = 0

    zwischenCode foreach println

  var paramCounter = 0
  for(code <- zwischenCode) code match {
    case PushMIntInstr(a) => if(!a.isInstanceOf[MIntImmediateValue]) paramCounter += 1
    case PushMAddressInstr(_) => paramCounter += 1
    case ObjectInstr(name) => listBuilder += ObjectDirective(name)
    case _ =>
  }
    listBuilder += ExecutableDirective("main")
    listBuilder += ExportDirective("main")

    var hasGlobalVars: Boolean = false
    for(code <- zwischenCode) code match {
      case AssignInstr(dest, operand1, op, operand2) =>

        dest match {
          case MIntProgLoc(infoDest) =>
            // GLOBALE VARIABLEN zuweisungen---------------------------------------------------------------------------
            if (infoDest.nesting == 0) {
              if (operand1.isDefined & operand2.isDefined) {
                solveAssignExp(operand1, op, operand2)
                setGlobal(infoDest,getValue(operand1),getValue(operand2))
              } else if(operand2.isDefined) {
                // AssignInstr(MIntProgLoc(RTLocInfo(0,0)),None,None,Some(MIntImmediateValue(5)))
                var t, t2 = acquireMIntTemp()
                listBuilder += Setw(t.nr, Left(getValue(operand2)))
                setGlobal(infoDest,t.nr,t2.nr)
                releaseMIntTemp(t)
                releaseMIntTemp(t2)
              } else if(operand1.isDefined) {
                // AssignInstr(MIntProgLoc(RTLocInfo(0,0)),Some(MIntProgLoc(RTLocInfo(0,-1))),None,None)
                operand1.get match {
                  case MIntProgLoc(infoOp) =>
                    var t,t2 = acquireMIntTemp()
                      if(infoOp.nesting == 0) {
                        //global = global
                        getGlobalValue(infoOp, t)
                        setGlobal(infoDest, t.nr, t2.nr)
                      } else if(infoOp.offset > 1) {
                        //global = parameter
                        getParameterValue(infoOp,t)
                        setGlobal(infoDest,t.nr,t2.nr)
                      }else {
                        // global = lokal
                        getLocalValue(infoDest,t)
                        setGlobal(infoOp,t.nr,t2.nr)
                      }
                    releaseMIntTemp(t)
                    releaseMIntTemp(t2)
                }
              }
              //--------------------------------------------------------------------------------------------------------
              // lokale Variablen paaraameter
            } else if (infoDest.offset > 1) {
              var t,t2 = acquireMIntTemp()
              listBuilder += Addc(t.nr,29,1 + (infoDest.offset - 2) * 4)
              if(operand1.isDefined & op.isEmpty) {
                operand1.get match {
                  case MIntProgLoc(info) =>
                    getParameterValue(info,t2)
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

            } else {
              if(operand1.isDefined & op.isDefined) {
                solveAssignExp(operand1,op,operand2)
                setLocal(infoDest,TempMIntLoc(getValue(operand1)),TempMIntLoc(getValue(operand2)))
              } else if(operand1.isDefined) {
                var t,t2 = acquireMIntTemp()
                operand1.get match {
                  case MIntProgLoc(infoOp) =>
                    if(infoOp.offset > 1){
                      //lokal = parameter
                      getParameterValue(infoOp,t)
                      setLocal(infoDest,t,t2)
                    } else if(infoOp.nesting == 0) {
                      //lokal = global
                      getGlobalValue(infoOp,t)
                      setLocal(infoDest,t,t2)
                    } else {
                      // lokal = lokal
                      getLocalValue(infoOp,t)
                      setLocal(infoDest,t,t2)
                    }
                    releaseMIntTemp(t)
                    releaseMIntTemp(t2)
                }
                releaseMIntTemp(t)
                releaseMIntTemp(t2)
              } else {
                var t,t2 = acquireMIntTemp()
                listBuilder += Setw(t.nr, Left(infoDest.offset*4-3))
                listBuilder += Add(t.nr, 29, t.nr)
                listBuilder += Setw(t2.nr, Left(getValue(operand2)))
                listBuilder += Stw(t2.nr, t.nr, 0)
                releaseMIntTemp(t)
                releaseMIntTemp(t2)
              }
            }
          case TempMIntLoc(nr) =>
            operand1 match {
              case Some(MIntProgLoc(locInfo)) =>
                if(locInfo.nesting == 0){
                  getGlobalValue(locInfo,TempMIntLoc(nr))
                } else if (locInfo.offset > 1) {
                  getParameterValue(locInfo,TempMIntLoc(nr))
                } else {
                  getLocalValue(locInfo,TempMIntLoc(nr))
                }
              case Some(DeRef(addrLoc)) =>
                addrLoc match {
                  case TempMAddressLoc(nrTemp) =>
                    listBuilder += Ldw(nrTemp,nrTemp,0)
                    listBuilder += Ldw(nr,nrTemp,0)
                }

              case None => listBuilder += Setw(nr, Left(getValue(operand2)))
            }

          case DeRef(addrLoc) =>
            operand1.get match {
              case MIntProgLoc(infoSource) =>
                listBuilder += Addc(getRegisterDeRef(addrLoc)+1,29,(infoSource.offset-2)*4+1)
                listBuilder += Stw(getRegisterDeRef(addrLoc)+1,tempMAddressLoc,0)
                listBuilder += Ldw(getRegisterDeRef(addrLoc),getRegisterDeRef(addrLoc),0)
                listBuilder += Ldw(getRegisterDeRef(addrLoc),getRegisterDeRef(addrLoc),0)
            }
          case _ =>

        }


      case WriteInstr(v) =>
        v match {
          case TempMIntLoc(nr) => listBuilder += Outi(nr)
          case MIntProgLoc(locInfo) =>
        }

      case ReadInstr(v) =>
        v match{
          case TempMIntLoc(nr) => listBuilder += Ini(nr)
        }


      case AssignAddrInstr(dest, source) =>
        dest match {
          case TempMAddressLoc(tempAddress) =>
            tempMAddressLoc = tempAddress
            source match {
              case MAddressProgLoc(infoSource) =>
                if(infoSource.nesting == 0){
                  getGlobalValue(infoSource,TempMIntLoc(tempAddress))
                } else if(infoSource.offset > 1) {
                  //getParameterValue(infoSource,TempMIntLoc(tempAddress))
                  listBuilder += Addc(tempAddress,29,1 + (infoSource.offset - 2) * 4)
                } else {
                  getLocalValue(infoSource,TempMIntLoc(tempAddress))
                }
              case MkRef(mIntLoc) =>
                mIntLoc match {
                  case MIntProgLoc(locInfo) =>
                    if(locInfo.nesting == 0){
                      listBuilder += Setw(tempAddress, Right("global_vars"))
                    } else if(locInfo.offset > 1) {
                      listBuilder += Addc(tempAddress,29,1 + (locInfo.offset - 2) * 4)
                    } else {
                      listBuilder += Setw(tempAddress,Left(locInfo.offset*4-3))
                      listBuilder += Add(tempAddress,29,tempAddress)
                    }
                }
            }

        }



      case IfInstr(operand1, op, operand2, jumpTo) =>
        solveBoolExp(operand1, op, operand2)
        listBuilder += Brt(getValue(Option(operand1)), jumpTo)


      case JumpInstr(label) => listBuilder += Jmp(label)

      case LabeledInstr(label) => listBuilder += Label(label)

      case ProcEntryInstr(label) =>
        functionparameters=countPushs(zwischenCode.indexOf(code))
        listBuilder += Label(label)
        //countParams(code)

      case CallInstr(callLabel) =>
        listBuilder += Call(30, callLabel)
        procTillCall = 0

      case ReturnInstr => listBuilder += Jmpr(30)

      case PushMIntInstr(t) =>
        t match{
          case TempMIntLoc(nr) =>
            listBuilder += Setw(nr+1,Left(procTillCall*4+1))
            listBuilder += Add(nr+1,nr+1,31)
            listBuilder += Stw(nr,nr+1,0)
            procTillCall += 1
          case MIntImmediateValue(nr) => listBuilder += Subc(31,31,4)
        }


      case PushMAddressInstr(a) =>
        a match{
          case TempMAddressLoc(nr) =>
            listBuilder += Setw(nr+1,Left(procTillCall*4+1))
            listBuilder += Add(nr+1,nr+1,31)
            listBuilder += Stw(nr,nr+1,0)
            procTillCall += 1
        }

      case PushCodeAddrInstr(returnLabel) =>

      case PushFPInstr =>
        listBuilder += Subc(31,31,8)
        listBuilder += Stw(29,31,1)
        listBuilder += Stw(30,31,5)
        listBuilder += Subc(31,31,functionparameters)

      case PopMIntInstr => listBuilder += Addc(31,31,4)

      case PopMAddressInstr => listBuilder += Addc(31, 31, 4)

      case PopCodeAddrToRRInstr =>
      //listBuilder += Addc(31,31,paramCounter*4)

      case PopFPInstr =>
        listBuilder += Ldw(30, 31, 5)
        listBuilder += Ldw(29, 31, 1)
        listBuilder += Addc(31,31,8)

      case StoreSPasFPInstr => listBuilder += Addc(29, 31, 0)

      case AllocStaticInstr(size) =>
        if (!hasGlobalVars) listBuilder += Label("global_vars")
        hasGlobalVars = true
        listBuilder += WordDirective(None)

      case _ =>
    }

    def countPushs(i: Int): Int = {
      var tmp = i
      var count = 0
      while(!zwischenCode(tmp).isInstanceOf[CallInstr]){
        if(zwischenCode(tmp).isInstanceOf[PushMIntInstr]) count += 1
        else if(zwischenCode(tmp).isInstanceOf[PushMAddressInstr]) count += 1
        tmp += 1
      }
      count * 4
    }

    def getGlobalValue(info: RTLocInfo,t: TempMIntLoc): Unit = {
      listBuilder += Setw(t.nr, Right("global_vars"))
      listBuilder += Ldw(t.nr,t.nr,-info.offset * 4)
    }
    def setGlobal(info: RTLocInfo,t: Int,t2: Int): Unit = {
      listBuilder += Setw(t2, Right("global_vars"))
      listBuilder += Addc(t2,t2,-info.offset * 4)
      listBuilder += Stw(t,t2,0)
    }
    def getLocalValue(info: RTLocInfo,t:TempMIntLoc): Unit = {
      listBuilder += Setw(t.nr,Left(info.offset*4-3))
      listBuilder += Add(t.nr,29,t.nr)
      listBuilder += Ldw(t.nr, t.nr, 0)
    }

    def setLocal(info: RTLocInfo,t: TempMIntLoc, t2: TempMIntLoc): Unit = {
      listBuilder += Setw(t2.nr, Left(info.offset*4-3))
      listBuilder += Add(t2.nr, 29, t2.nr)
      listBuilder += Stw(t.nr, t2.nr, 0)
    }

    def getParameterValue(info: RTLocInfo,t: TempMIntLoc): Unit = {
      listBuilder += Addc(t.nr,29,1 + (info.offset - 2) * 4)
      listBuilder += Ldw(t.nr,t.nr,0)
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

