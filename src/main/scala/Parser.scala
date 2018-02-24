import AST._
import ExpScanner.{NumberToken, Token}

import scala.util.parsing.combinator.JavaTokenParsers
/* TOKENS IN SÄTZE (TRAVERSIEREN)*/

object Parser {
  def parse(code : List[(Int, Token)]) : List[(Int, Code)] = {
    var result: List[(Int, Code)] = List()

    for (i <- code.indices) {
      println(code(i)._2)
      if(code(i)._2.isInstanceOf[NumberToken]) Number(num <: code(i)._2)
    }
    result
  }
}

