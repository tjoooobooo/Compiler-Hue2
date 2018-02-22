package mini_puck_c.compiler

import scala.util.parsing.input.Positional

object AST {

  sealed abstract class BoolExp extends Positional
  case class Less(l: Exp, r: Exp) extends BoolExp
  case class Greater(l: Exp, r: Exp) extends BoolExp
  case class Equal(l: Exp, r: Exp) extends BoolExp
  case class NotEq(l: Exp, r: Exp) extends BoolExp
  case class LessEq(l: Exp, r: Exp) extends BoolExp
  case class GreaterEq(l: Exp, r: Exp) extends BoolExp

  sealed abstract class Exp extends Positional
  case class Number(d: Int)        extends Exp
  case class Add(e1: Exp, e2: Exp) extends Exp
  case class Sub(e1: Exp, e2: Exp) extends Exp
  case class Div(e1: Exp, e2: Exp) extends Exp
  case class Mul(e1: Exp, e2: Exp) extends Exp


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

}
