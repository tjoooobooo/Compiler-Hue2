package ZwischenCode
import ZwischenAST._

object ZwischenCodePrinter {

  def print(parsedCode : List[Instr]) : String = {
    var s = ""
    for(code <- parsedCode) {
      code match {
        case temp: AssignInstr =>

          var value = 0
          var assignLoc = 0
          var op1 = 0
          var op2 = 0
          var v = ""
          temp.operand2 match {
            case Some(MIntImmediateValue(d)) => value= d
            case _ =>
          }
          temp.dest match {
            case Variable(name,loc) =>
              v = name
            case TempMIntLoc(nr) => assignLoc = nr
            case _ =>
          }
          temp.operand1 match {
            case Some(TempMIntLoc(nr)) => op1 = nr
            case _ =>
          }
          temp.operand2 match {
            case Some(TempMIntLoc(nr)) => op2 = nr
            case _ =>
          }


          temp.op match {
            case Some(AddOp) =>
              s += "t"+ assignLoc + " := "  + "t"+op1 + " + " + "t"+op2 + "\n"
            case Some(SubOp) =>
              s += "t"+ assignLoc + " := "  + "t"+op1 + " - " + "t"+op2 + "\n"
            case Some(MultOp) =>
              s += "t"+ assignLoc + " := "  + "t"+op1 + " * " + "t"+op2
            case Some(DivOp) =>
              s += "t"+ assignLoc + " := "  + "t"+op1 + " / " + "t"+op2 + "\n"
            case _ => s += "t"+assignLoc + " := " + value + "\n"
          }


        case _ =>
      }
    }
    println(s)
    s
  }

}
