
import ZwischenCode._
import backend.Evaluator
import frontend.AST._
import frontend.ProgParsers
import frontend.ProgSymbols.VarSymbol


object Main {
  //TODO wir brauchen noch irgendeine Main

  def main(args: Array[String]): Unit = {
    var pfad = "PuckTest//"
    var puckFile = scala.io.Source.fromFile(pfad + "test.puck").mkString
    //TODO proc aufruf was mit ref da
    var parsed = ProgParsers.parse(puckFile)
    var res = ZwischenCode.ZwischenCodeGenerator.translate(parsed)
    CodeGenerator.GenAssemblerLines.gen(res)
  }

}
