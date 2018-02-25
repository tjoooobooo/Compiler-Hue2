import compiler.AST._
import frontend.ExpScanner.{NumberToken, Token}

import scala.util.parsing.combinator.JavaTokenParsers
/* TOKENS IN SÄTZE (TRAVERSIEREN)*/

object Parser {
  def parse(code : List[(Int, Token)]) : List[(Int, Code)] = {
    var result: List[(Int, Code)] = List()

    for (i <- code.indices) {
      var token = code(i)._2
      println(token)
      if(token.isInstanceOf[NumberToken]) (NumberToken)token
    }
    result
  }
}

