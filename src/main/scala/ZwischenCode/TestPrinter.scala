package ZwischenCode
import ZwischenAST._

object TestPrinter {
  def print(parsedCode: List[Instr]): String = {
    var s = ""
    for (code <- parsedCode) {
      code match {
        case temp: AssignInstr =>
          var reg = 0
          var op1 = 0
          var opV1 = ""
          var vName = ""


          temp.dest match {
            case Variable(name, loc) =>
              vName = name
              loc match {
                case TempMIntLoc(nr) =>
                  reg = nr
                  s += "t" + reg + " := " + vName + " = "
              }
            case TempMIntLoc(nr) =>
              reg = nr
              s += "t" + reg + " := "
          }

          temp.operand1 match {
            case Some(TempMIntLoc(nr)) =>
              op1 = nr
              s += "t" + op1
            case Some(Variable(name, loc)) =>
              loc match {
                case TempMIntLoc(nr) =>
                  op1 = nr
                  s += "t" + op1
              }
            case None =>
          }

          temp.op match {
            case Some(AddOp) => s += " + "
            case Some(SubOp) => s += " - "
            case Some(MultOp) => s += " * "
            case Some(DivOp) => s += " / "
            case None =>
          }

          temp.operand2 match {
            case Some(TempMIntLoc(nr)) =>
              op1 = nr
              s += "t" + op1 + "\n"
            case Some(Variable(name, loc)) =>
              loc match {
                case TempMIntLoc(nr) =>
                  op1 = nr
                  s += "t" + op1 + "\n"
              }
            case Some(MIntImmediateValue(d)) =>
              s += d + "\n"
            case None => s += "\n"

          }

            case _ =>

          }
      }
    println(s)
    s
  }
}
