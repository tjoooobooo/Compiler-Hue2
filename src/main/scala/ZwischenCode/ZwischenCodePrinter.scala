package ZwischenCode
import ZwischenAST._

object ZwischenCodePrinter {

  def print(parsedCode : List[Instr]) : Unit = {
    for(code <- parsedCode) {
      code match {
        case temp: AssignInstr =>

          var value = 0
          var assignLoc = 0
          var op1 = 0
          var op2 = 0

          temp.operand2 match {
            case Some(MIntImmediateValue(d)) => value= d
            case _ =>
          }
          temp.dest match {
            case TempLoc(nr) => assignLoc = nr
            case _ =>
          }
          temp.operand1 match {
            case Some(TempLoc(nr)) => op1 = nr
            case _ =>
          }
          temp.operand2 match {
            case Some(TempLoc(nr)) => op2 = nr
            case _ =>
          }

          temp.op match {
            case Some(AddOp) =>
              println("t"+ assignLoc + " := "  + "t"+op1 + " + " + "t"+op2)
            case Some(SubOp) =>
              println("t"+ assignLoc + " := "  + "t"+op1 + " - " + "t"+op2)
            case Some(MultOp) =>
              println("t"+ assignLoc + " := "  + "t"+op1 + " * " + "t"+op2)
            case Some(DivOp) =>
              println("t"+ assignLoc + " := "  + "t"+op1 + " / " + "t"+op2)
            case _ => println("t"+assignLoc + " := " + value)
          }

        case _ =>
      }
    }
  }

}
