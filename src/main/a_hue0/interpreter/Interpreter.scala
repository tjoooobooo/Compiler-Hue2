package thm.mni.cb1718.hue_0.interpreter

import scala.util.Try


object Interpreter {

  private def interpret(text: String): Int = eval(text)

  private def eval(text: String): Int = {
    var pos: Int = 0
    var num : String = ""
    def evalNumber(s: String): Int = {
      Integer.parseInt(s)
    }

    def evalOperation(): Int = {
      pos = pos+1 // skip '('
      val exp1 = evalExp
      while(text(pos) == ' ') pos = pos+1
      val opSymbol = text(pos)
      pos = pos+1 // skip opSign
      val exp2 = evalExp
      pos = pos+1 // skip ')'
      opSymbol match {
        case '+' => exp1 + exp2
        case '-' => exp1 - exp2
        case '*' => exp1 * exp2
        case '/' => exp1 / exp2
        case _ => throw new IllegalArgumentException
      }
    }

      def evalExp: Int = text(pos) match {
        case v @ (' ') =>
          pos = pos+1
          evalExp
        case v @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
          pos = pos+1
          val sb : StringBuilder = new StringBuilder()
          sb.append(v)
          while(Try (Integer.parseInt(text(pos).toString)).isSuccess || text(pos) == ' '){
            if (text(pos) == ' ') pos = pos + 1
            else {sb.append(text(pos))
            pos = pos+1}
          }
          var s : String = sb.toString
          evalNumber(s)
        case '(' => evalOperation()
        case _ => throw new IllegalArgumentException
      }
      evalExp
  }


  def run(): Unit = {
    var expression = ""
    do {
      println("Enter expression, to stop hit Return")
      expression = scala.io.StdIn.readLine()
      if (expression != "") {
        println(interpret(expression))
      }
    } while (expression != "")
  }

}
