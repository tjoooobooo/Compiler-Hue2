
import ZwischenCode.{TestPrinter, ZwischenCodePrinter}
import backend.Evaluator
import frontend.AST._
import frontend.ProgParsers
import frontend.ProgSymbols.VarSymbol


object Main {

  def main(args: Array[String]): Unit = {
    var pfad = "PuckTest//"
    var puckFile = scala.io.Source.fromFile(pfad + "test.puck").mkString
    var parsed = ProgParsers.parse(puckFile)
    println(parsed.defList)
    println(parsed.cmdList)
    println("---------------")
    val exp: Cmd = Assign(DirectLoc(VarSymbol("a")),Mul(Add(Number(5),Number(7)),Sub(Number(8),Number(2))))
    //val exp: Exp = Mul(Add(Number(1), Number(2)), Number(3))

    val instruk = ZwischenCode.ZwischenCode.genCode(parsed)
    instruk.foreach{println}
    //ZwischenCodePrinter.print(instruk)
    TestPrinter.print(instruk)
  }

}
