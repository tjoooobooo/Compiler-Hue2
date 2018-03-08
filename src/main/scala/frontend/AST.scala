package frontend

import frontend.StaticTypes.TypeInfo
import ProgSymbols._

import scala.util.parsing.input.Positional

object AST {

  sealed abstract class BoolExp extends Positional
  case class Less(l: Exp, r: Exp) extends BoolExp
  case class Greater(l: Exp, r: Exp) extends BoolExp
  case class Equal(l: Exp, r: Exp) extends BoolExp
  case class NotEq(l: Exp, r: Exp) extends BoolExp
  case class LessEq(l: Exp, r: Exp) extends BoolExp
  case class GreaterEq(l: Exp, r: Exp) extends BoolExp

  /* BITWISE OPERATOREN*/
  sealed abstract class BitwiseExp extends Exp
  case class And(l: Exp, r: Exp) extends BitwiseExp
  case class Or(l: Exp, r: Exp) extends BitwiseExp
  case class Xor(l: Exp, r: Exp) extends BitwiseExp
  case class Sl(l: Exp, r: Exp) extends BitwiseExp
  case class Sr(l: Exp, r: Exp) extends BitwiseExp

  sealed abstract class Exp extends Positional {
    var staticType: Option[TypeInfo] = None // will be set by typifier
  }
  case class Number(d: Int)        extends Exp
  case class Add(e1: Exp, e2: Exp) extends Exp
  case class Sub(e1: Exp, e2: Exp) extends Exp
  case class Div(e1: Exp, e2: Exp) extends Exp
  case class Mul(e1: Exp, e2: Exp) extends Exp
  case class Mod(e1: Exp, e2: Exp) extends Exp
  case class LocAccess(var locExp: LocExp)   extends Exp
  // TODO ARG IRGENDWAS
  case class Arg(exp: Exp, var method: Option[ParamPassMethod] = None )
  sealed abstract class ParamPassMethod
  case object ByValue extends ParamPassMethod
  case object ByRef extends ParamPassMethod

  sealed abstract class LocExp extends Positional {
    var staticType: Option[TypeInfo] = None // will be set by typifier
  }
  case class DirectLoc(symb: LocSymbol) extends LocExp

  case class StarConv(locExp: LocExp) extends LocExp

  //sealed abstract class Var extends Positional

  case class Prog(defList: List[Definition], cmdList: List[Cmd])

  sealed abstract class TypeExp  extends Positional
  case object IntTypeExp extends TypeExp

  // Commands  ---------------------------------------------------------------------------------------------------------
  sealed abstract class Cmd extends Exp
  case class Assign(var left: LocExp, right: Exp) extends Cmd
  case class If(e: BoolExp, thenCmds: List[Cmd], elseCmds: List[Cmd]) extends Cmd
  case class While(e: BoolExp, cmds:  List[Cmd]) extends Cmd
  case class Write(e:Exp) extends Cmd
  case class Call(symb: ProcSymbol, args: List[Arg]) extends Cmd
  case class Comment(c: String) extends Cmd

  // Definitions  ------------------------------------------------------------------------------------------------------
  // all definitions introduce symbols
  sealed abstract class Definition extends Positional {
    type SymbType <: ProgSymbol
    val symb: SymbType
  }
  case class VarDef(
                     override val symb: VarSymbol,
                     t: TypeExp,  // the declared type
                     e: Exp       // the initializing expression
                   ) extends Definition {
    type SymbType = VarSymbol
  }
  case class ProcDef(
                      override val symb: ProcSymbol,
                      fparams: List[ParamDef],
                      locals:  List[VarDef],
                      cmds:    List[Cmd]
                    ) extends Definition {
    type SymbType = ProcSymbol
  }

  sealed abstract class ParamDef extends Definition {
    val symb: ParamSymbol
    type SymbType = ParamSymbol
  }

  case class ValueParamDef(
                            override val symb: ParamSymbol,
                            t: TypeExp
                          ) extends ParamDef

  case class RefParamDef (
                           override val symb: ParamSymbol,
                           t: TypeExp
                         ) extends ParamDef

  // Objects
  case class Obj(
                  name: String,
                  imports: List[(String, ProgSymbol)],  // imported Symbols: object name and symbol from that object
                  exports: List[String],      // Names of symbols that have to be exported after context analysis
                  defs: List[Definition],
                  cmds: List[Cmd])
}

