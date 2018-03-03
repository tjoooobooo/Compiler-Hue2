package CodeGenerator

import scala.util.Try

object ZwischenParser {
  private val opPatS = """(\+|\-|\*|\/)"""
  private val operationPatS = """t([0-9][0-9]?) := t([0-9][0-9]?) """ + (opPatS) + """ t([0-9][0-9]?)"""
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
      var token = nexToken
      //Macht am ende einmal zu oft \n
      result += "\n"
    }

    def nexToken: Unit = {
      tokens.next() match {
        case OperationPat(t1,t2,op,t3) =>
          op match {
            case "+" => result += "ADD" + " $" + t1 + " $" + t2 + " $" + t3
            case "-" => result += "SUB" + " $" + t1 + " $" + t2 + " $" + t3
            case "/" => result += "MUL" + " $" + t1 + " $" + t2 + " $" + t3
            case "*" => result += "DIV" + " $" + t1 + " $" + t2 + " $" + t3
          }
        case ZuweisungPat(t1,num) => result += "SETW" + " $" + t1 + " " + num
        case _=>

      }
    }
    println(result)
    result
  }

}
