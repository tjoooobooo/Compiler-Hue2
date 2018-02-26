package a_slides_10.frontend

import slides_10.frontend.ProgSymbols
import slides_10.frontend.ProgSymbols.{ProgSymbol, Variable}

import scala.util.parsing.input.Positional

object AST {
  sealed abstract class Exp extends Positional

  sealed abstract class BoolExp extends Exp
  case class Less(l: Exp, r: Exp) extends BoolExp
  case class Greater(l: Exp, r: Exp) extends BoolExp
  case class Equal(l: Exp, r: Exp) extends BoolExp
  case class NotEq(l: Exp, r: Exp) extends BoolExp
  case class LessEq(l: Exp, r: Exp) extends BoolExp
  case class GreaterEq(l: Exp, r: Exp) extends BoolExp

  sealed abstract class ArithExp extends Exp
  case class Number(d: Int)        extends ArithExp
  case class Add(e1: Exp, e2: Exp) extends ArithExp
  case class Sub(e1: Exp, e2: Exp) extends ArithExp
  case class Div(e1: Exp, e2: Exp) extends ArithExp
  case class Mul(e1: Exp, e2: Exp) extends ArithExp

  sealed abstract class RefExp extends ArithExp
  case class Ref(l: RefExp) extends RefExp

  sealed abstract class Var extends Positional

  case class Prog(defList: List[Definition], cmdList: List[Any])

  sealed  abstract class Cmd

  case class If(e: BoolExp, cthen: List[Cmd], cElse: List[Cmd]) extends Cmd
  case class While(e: BoolExp, cmdList: List[Cmd]) extends Cmd
  case class Write(e: Exp) extends Cmd
  case class Assign(ref: RefExp, e: Exp) extends Cmd


  sealed abstract class Definition extends Positional {
    type SymbType <: ProgSymbol
    val symb: SymbType
  }
  case class VarRef(
                     name: String
                   ) extends RefExp
  case class VarRef2(variable: ProgSymbols.Variable) extends RefExp
  case class VarDef2(
                     override val symb: Variable,
                     //t: TypeExp,  // the declared type
                     e: Exp       // the initializing ArithExpression
                   ) extends Definition {
    type SymbType = ProgSymbols.Variable
  }
  abstract case class VarDef(vari: ProgSymbols.Variable, e: Exp) extends Definition
  sealed abstract class Obj extends Positional
  case class ProcDef(value: Any, paramdefs: Any, localdefs: Any, value1: Any) extends Obj

}
