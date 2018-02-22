import StaticTypes.StaticType

object AST {
  type Object = List[Code]
  trait Code

  sealed abstract class Exp extends Code{
    var staticType: Option[StaticType] = None // will be set by typifier
}

  case class Number(d: Int) extends Exp
  case class StrLit(str: String) extends Exp
  case class Add(e1: Exp, e2: Exp) extends Exp
  case class Sub(e1: Exp, e2: Exp) extends Exp
  case class Div(e1: Exp, e2: Exp) extends Exp
  case class Mul(e1: Exp, e2: Exp) extends Exp
  case class Ref(ref: RefExp) extends Exp

  // Expression that denote references to storage locations
  sealed abstract class RefExp {
    var staticType: Option[StaticType] = None // will be set by typifier
  }

}
