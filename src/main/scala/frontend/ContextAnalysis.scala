package frontend

import ZwischenCode.ZwischenCodeAST.MIntProgLoc
import frontend.AST._
import frontend.ProgSymbols.{ParamSymbol, RefParamSymbol, ValParamSymbol}

import scala.collection.mutable.ListBuffer

object ContextAnalysis {

  def checkContext(obj: Obj): Obj = {
    var resDefinitons: ListBuffer[Definition] = ListBuffer()
    var resCommands: ListBuffer[Cmd] = ListBuffer()

    for(definiton <- obj.defs) resDefinitons += transformDef(definiton)
    for(cmd <- obj.cmds) resCommands += transformCmd(cmd)

    def transformCmd(cmdsVal: Cmd): Cmd = cmdsVal match {
      case Call(symb,args) =>
        if(symb.params.head.lengthCompare(args.size) != 0)
          throw new IllegalArgumentException("False number of Parameters in Call " + symb.name + "()")
        var res : ListBuffer[Arg] = new ListBuffer[Arg]()
        for(arg <- args.indices) args(arg).exp match {
          case LocAccess(f) =>
            f match {
              case DirectLoc(symb2) =>
                symb.params.get(arg) match {
                case RefParamSymbol(name) =>
                  res += Arg(args(arg).exp, Some(ByRef))
                case _ => res += Arg(args(arg).exp,Some(ByValue))
              }
              case _ => res += Arg(args(arg).exp,Some(ByValue))
            }
          case Number(f) =>
            symb.params.get(arg) match {
              case RefParamSymbol(name) => throw new IllegalArgumentException("Value "+f+" is no reference in call " + symb.name)
              case _ => res += Arg(args(arg).exp, Some(ByValue))

            }
        }
        Call(symb, res.toList)
      case Assign(left,right) =>
      left match {
        case DirectLoc(symb) =>
          if(symb.isInstanceOf[ValParamSymbol]) throw new IllegalArgumentException("Reassignment to val " + symb.name)
          else Assign(left,right)
        case _ => Assign(left,right)
        }
      case While(e,cmds) => While(e,cmds)
      case If(e,thenCmds,Nil) => If(e,thenCmds,Nil)
      case If(e,thenCmds,elseCmds) => If(e,thenCmds,elseCmds)
      case Write(e) => Write(e)
      case Read(e) => Read(e)
    }

    def transformDef(definition: Definition): Definition = definition match {
      case VarDef(symb,t,e) =>
        checkVariable(symb,t,e)
      case ProcDef(symb,fparams,locals,cmds) =>
        checkProc(symb,fparams,locals,cmds)
    }

    def checkVariable(symbol: ProgSymbols.VarSymbol, exp: AST.TypeExp, maybeExp: Option[AST.Exp]): VarDef ={
      VarDef(symbol,exp,maybeExp)
    }

    def checkProc(symbol: ProgSymbols.ProcSymbol, defs: List[AST.ParamDef], defs1: List[AST.VarDef], cmds: List[AST.Cmd]): ProcDef = {
      var procCommands: ListBuffer[Cmd] = ListBuffer()
      for(cmd <- cmds) procCommands += transformCmd(cmd)
      ProcDef(symbol,defs,defs1,procCommands.toList)
    }

    // return transformed AST
    Obj(obj.name,None,None,resDefinitons.toList,resCommands.toList)
  }

}
