package ZwischenCode
import ZwischenAST._

object TestPrinter {
  def print(parsedCode: List[Instr]): String = {
    var s = ""
    for (code <- parsedCode) {
      code match {
        case temp: AssignInstr =>
          var v = ""
          var reg = 0

          temp.dest match {
            case Variable(name, loc) =>
              v = name
          }

      }
    }
    s
  }
}
