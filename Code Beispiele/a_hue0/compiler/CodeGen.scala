package thm.mni.cb1718.hue_0.compiler

import thm.mni.cb1718.hue_0.{ExpTree, Operation, Number}

object CodeGen {


  def genCode(tree: ExpTree): List[Instruction] = tree match {
    case Number(x) => List(Pushc(x))
    case Operation(op, exp1, exp2) =>
      val l1 = genCode(exp1)
      val l2 = genCode(exp2)
      l1 ::: l2 ::: (op match {

        case '+' => List(Add)
        case '-' => List(Sub)
        case '*' => List(Mult)
        case '/' => List(Div)
      })
  }
}