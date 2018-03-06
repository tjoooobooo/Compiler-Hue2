package compiler

object CompilerString {
  private val opPatS = """(\+|\-|\*|\/)"""
  private val operationPatS = """t([0-9][0-9]?) := t([0-9][0-9]?) """ + opPatS + """ t([0-9][0-9]?)"""
  private val zuweisungPatS = """t([0-9][0-9]?) := ([0-9][0-9]?)"""



  private val OperationPat = operationPatS.r
  private val ZuweisungPat = zuweisungPatS.r
  private val OpPat = opPatS

  private val tokenPatS = List(operationPatS, zuweisungPatS).reduceRight(_ + "|" + _)
  private val tokenPat = tokenPatS.r

  def apply(code: String): String = {
    var result = ""
    var tokens = tokenPat.findAllIn(code)


    var counter: Int = 1
    while (tokens.hasNext) {
      //Macht am ende einmal zu oft \n
      result += nexToken
      result += "\n"
    }

    def nexToken: String = {
      tokens.next() match {
        case OperationPat(t1,t2,op,t3) =>
          op match {
            case "+" => "ADD" + " $" + t1 + " $" + t2 + " $" + t3
            case "-" => "SUB" + " $" + t1 + " $" + t2 + " $" + t3
            case "/" => "MUL" + " $" + t1 + " $" + t2 + " $" + t3
            case "*" => "DIV" + " $" + t1 + " $" + t2 + " $" + t3
          }
        case ZuweisungPat(t1,num) => "SETW" + " $" + t1 + " " + num
        case _=> ""

      }
    }
    println(result)
    result
  }

}
