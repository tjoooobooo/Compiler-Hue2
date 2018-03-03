package ZwischenCode
import ZwischenAST._

object TestPrinter {
  def print(parsedCode: List[Instr]): String = {
    var s = ""
    for (code <- parsedCode) {
      code match {
        case temp: AssignInstr =>
          temp.dest match {
            case Variable(name, loc) =>

              loc match {
                case TempMIntLoc(nr) =>
                  s += "t" + nr + " := " + name + " = "
              }
            case TempMIntLoc(nr) =>
              s += "t" + nr + " := "
          }

          temp.operand1 match {
            case Some(TempMIntLoc(nr)) =>

              s += "t" + nr
            case Some(Variable(name, loc)) =>
              loc match {
                case TempMIntLoc(nr) =>
                  s += "t" + nr
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
              s += "t" + nr + "\n"
            case Some(Variable(name, loc)) =>
              loc match {
                case TempMIntLoc(nr) =>
                  s += "t" + nr + "\n"
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
