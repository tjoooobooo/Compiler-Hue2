import scala.util.parsing.input.Positional

object AST {

  sealed abstract class Exp extends Positional
  case class Number(d: Int)        extends Exp
  case class Add(e1: Exp, e2: Exp) extends Exp
  case class Sub(e1: Exp, e2: Exp) extends Exp
  case class Div(e1: Exp, e2: Exp) extends Exp
  case class Mul(e1: Exp, e2: Exp) extends Exp

}
