
import ZwischenCode.{ZwischenGen, ZwischenPrinter}
import backend.Evaluator
import compiler.{AbstractSyntaxPrinter, CompilerListe}
import frontend.AST._
import frontend.ProgParsers
import frontend.ProgSymbols.VarSymbol


object Main {

  def main(args: Array[String]): Unit = {
    var pfad = "PuckTest//"
    var puckFile = scala.io.Source.fromFile(pfad + "test.puck").mkString
    //TODO proc aufruf was mit ref da
    var parsed = ProgParsers.parse(puckFile)
    parsed.defList.foreach{println}
    println(parsed.cmdList)
    println("------------------")

    val instruk = ZwischenGen.genCode(parsed)
    instruk.foreach{println}
  }

}
