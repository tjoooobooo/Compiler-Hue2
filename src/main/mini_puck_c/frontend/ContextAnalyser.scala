package mini_puck_c.frontend

import mini_puck_c.compiler.AST._
import mini_puck_c.compiler.AstTraversal.{AllCmds, AllDefs}
import mini_puck_c.backend.RuntimeOrganisation
import mini_puck_c.frontend.ProgSymbols.{RefParamSymbol, ValParamSymbol, VarSymbol}
import mini_puck_c.frontend.StaticTypes.{IntTypeInfo, RefTypeInfo, TypeInfo}


import scala.util.Try

/**
  * This object provides functions that are used during context analysis.
  */
object ContextAnalyser {

  private def evalTypeExp(te: TypeExp): TypeInfo = te match {
    case IntTypeExp => IntTypeInfo
  }

  private def analyseExp(e: Exp): Unit = {
    def checkAndSetType(l: Exp, r: Exp): Unit = {

      analyseExp(l)
      analyseExp(r)

      (l.staticType, r.staticType) match {
        case (None, _) => throw new Exception(s"internal error 2: type not set for $l")
        case (_, None) => throw new Exception(s"internal error 3: type not set for $r")
        case (Some(st1), Some(st2)) =>
          if (st1 == st2)
            e.staticType = Some(st1)
          else throw new Throwable(s"Type error at ${e.pos} types are compatible")
      }
    }

    e match {
      case Number(_) =>
        e.staticType = Some(IntTypeInfo)
      case Add(e1: Exp, e2: Exp) =>
        analyseExp(e1)
        analyseExp(e2)
        checkAndSetType(e1, e2)
      case Sub(e1: Exp, e2: Exp) =>
        analyseExp(e1)
        analyseExp(e2)
        checkAndSetType(e1, e2)
      case Div(e1: Exp, e2: Exp) =>
        analyseExp(e1)
        analyseExp(e2)
        checkAndSetType(e1, e2)
      case Mul(e1: Exp, e2: Exp) =>
        analyseExp(e1)
        analyseExp(e2)
        checkAndSetType(e1, e2)
      case LocAccess(ref: LocExp) =>
        analyseLocExp(ref)
        e.staticType = ref.staticType
    }
  }

  private def analyseBE(be: BoolExp): Unit = {

    def checkForIntType(l: Exp, r: Exp): Unit = {
      (l.staticType, r.staticType) match {
        case (None, _) => throw new Exception(s"internal error 4: type not set")
        case (_, None) => throw new Exception(s"internal error 5: type not set")
        case (Some(IntTypeInfo), Some(IntTypeInfo)) => // OK
        case (_, _) => throw new Throwable(s"Type error at ${be.pos} expressions may not be compared")
      }
    }

    be match {
      case Less(l, r) =>
        analyseExp(l)
        analyseExp(r)
        checkForIntType(l, r)
      case Greater(l, r) =>
        analyseExp(l)
        analyseExp(r)
        checkForIntType(l, r)
      case Equal(l, r) =>
        analyseExp(l)
        analyseExp(r)
        checkForIntType(l, r)
      case NotEq(l, r) =>
        analyseExp(l)
        analyseExp(r)
        checkForIntType(l, r)
      case LessEq(l, r) =>
        analyseExp(l)
        analyseExp(r)
        checkForIntType(l, r)
      case GreaterEq(l, r) =>
        analyseExp(l)
        analyseExp(r)
        checkForIntType(l, r)
    }
  }


  //--------------------------------------------------


  private def anlyseTypes(obj: Obj) =
    AllDefs(obj).traverseWith {
      case VarDef(symb@VarSymbol(_), typeExp, _) =>
        symb.staticType = Some(evalTypeExp(typeExp))

      case ProcDef(procSymb, fparams, locals, _) =>
        fparams.foreach {

          case RefParamDef(paramSymb, typeExp) =>
            paramSymb.staticType = Some(RefTypeInfo(evalTypeExp(typeExp)))

          case ValueParamDef(varSymb, typeExp) =>
            varSymb.staticType = Some(evalTypeExp(typeExp))
        }
        locals.foreach {
          case VarDef(symb@VarSymbol(_), typeExp, _) =>
            symb.staticType = Some(evalTypeExp(typeExp))
        }
        // put static info about procedure into symbol
        procSymb.params = Some(fparams.map(_.symb))
        procSymb.locals = Some(locals.map(_.symb))
      case ValueParamDef(_, _) => /* should never match: not a toplevel definition */
      case RefParamDef(_, _) => /* should never match: not a toplevel definition */
    }

  private def analyseLocExp(le: LocExp): Unit = le match {
    case DirectLoc(v@VarSymbol(_)) =>
      le.staticType = v.staticType
    case DirectLoc(pv@ValParamSymbol(_)) =>
      le.staticType = pv.staticType
    case DirectLoc(pr@RefParamSymbol(_)) =>
      le.staticType = pr.staticType
    case StarConv(sub_le) =>
      analyseLocExp(sub_le)
      sub_le.staticType match {
        case Some(RefTypeInfo(baseType)) =>
          le.staticType = Some(baseType)
        case _ =>
          throw new Exception(s"internal error: $le with type ${le.staticType} may not be de-referenced")
      }
  }


  private def analyseInits(obj: Obj) = AllDefs(obj).traverseWith {
      case VarDef(VarSymbol(_), typeExp, e) =>
        analyseExp(e)
        (e.staticType, evalTypeExp(typeExp)) match {
          case (None, _) => throw new Exception(s"internal error 1: type of var not set for $e")
          case (Some(IntTypeInfo), IntTypeInfo) => // OK
          case (Some(_), _) => throw new Throwable("Type mismatch in init statement")
        }
      case _ => // ignore: no inits
    }


  private def analyseCmds(obj: Obj) = AllCmds(obj).traverseWith {
    case cmd@Assign(left, right) =>
      analyseLocExp(left)
      analyseExp(right)
      (left.staticType, right.staticType) match {
        case (None, _) =>
          throw new Exception(s"internal error 6: type not set")
        case (_, None) =>
          throw new Exception(s"internal error 7: type not set")
        case (Some(IntTypeInfo), Some(IntTypeInfo)) =>
        // OK only ints are assignable
        case (_, _) => throw new Throwable(s"Type error at ${cmd.pos} incompatible types in assignment")
      }

    case If(be, _, _) =>
      analyseBE(be)
      // sub-commands will be treated on their own


    case While(be, _) =>
      analyseBE(be)
      // sub-commands will be treated on their own

    case Write(e) =>
      analyseExp(e)

    case call@Call(symb, args) =>
      if (symb.params.get.length != args.length) {
        throw new Throwable(s"argument list does not match parameter list at ${call.pos}")
      }
      args.foreach {
        case Arg(e, None) =>
          analyseExp(e)
        case arg@Arg(_, _) => throw new Exception(s"internal error $arg already has pp-method assigned")
      }
      symb.params.get.zip(args).foreach {
        case (ValParamSymbol(_), arg@Arg(_, None)) =>
          arg.method = Some(ByValue)
        case (RefParamSymbol(_), arg@Arg(e, None)) =>
          e match {
            case LocAccess(_) =>
              arg.method = Some(ByRef)
            case _ => throw new Throwable(s"Only variables may be passed by reference at ${call.pos}")
          }
        case (_, arg@Arg(_, _)) => throw new Exception(s"internal error $arg already has pp-method assigned")
      }

  }


  private def frameLayout(obj: Obj) = AllDefs(obj).traverseWith {
    case ProcDef(procSymb, _, _, _) =>
      RuntimeOrganisation.frameLayout(1, procSymb)
    case _ => // ignore
  }

  def analyseObj(obj: Obj): Try[Unit] =
    for ( _ <- anlyseTypes(obj);
          _ <- analyseInits(obj);
          _ <- analyseCmds(obj);
          _ <- frameLayout(obj)) yield {
      val topLevelVarSymbols: List[VarSymbol] = obj.defs.flatMap {
        case VarDef(vs,_,_) => List(vs)
        case _ => Nil
      }
      RuntimeOrganisation.topLevelLayout(topLevelVarSymbols)
    }
}

