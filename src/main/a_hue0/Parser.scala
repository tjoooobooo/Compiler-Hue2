package thm.mni.cb1718.hue_0

import scala.util.Try

object Parser {

  def parse(text: String): ExpTree = {
    var pos: Int = 0

    def parseExp: ExpTree = text(pos) match {
      case v @ (' ') =>
        pos = pos+1
        parseExp
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  => parseNumber
      case '(' => parseOperation
      case _ => throw new IllegalArgumentException
    }

    def parseOperation: Operation = {
      pos = pos+1 // skip '('
      val exp1 = parseExp
      while(text(pos) == ' ') pos = pos+1
      val opSymbol = text(pos)
      pos = pos+1 // skip opSign
      val exp2 = parseExp
      pos = pos+1 // skip ')'
      opSymbol match {
        case '+' => Operation(opSymbol, exp1, exp2)
        case '-' => Operation(opSymbol, exp1, exp2)
        case '*' => Operation(opSymbol, exp1, exp2)
        case '/' => Operation(opSymbol, exp1, exp2)
        case _ => throw new IllegalArgumentException
      }
    }

    def parseNumber: Number = text(pos) match {
      case v @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ) =>
        pos = pos+1
        val sb : StringBuilder = new StringBuilder()
        sb.append(v)
        while(Try (Integer.parseInt(text(pos).toString)).isSuccess || text(pos) == ' '){
          if (text(pos) == ' ') pos = pos + 1
          else {sb.append(text(pos))
            pos = pos+1}
        }
        var s : String = sb.toString
        Number(Integer.parseInt(s))
      case _ => throw new IllegalArgumentException
    }

    parseExp
  }

}
