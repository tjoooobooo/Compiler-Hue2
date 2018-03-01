
import CodeGenerator.{ZwischenCode, ZwischenCodePrinter}
import CodeGenerator.ZwischenCode.{AssignInstr, TempLoc}
import frontend.AST._
import frontend.ProgParsers

object Main {

  def main(args: Array[String]): Unit = {
    var pfad = "PuckTest//"
    var puckFile = scala.io.Source.fromFile(pfad + "test.puck").mkString
    var parsed = ProgParsers.parse(puckFile)
    parsed.defList.foreach{println}
    parsed.cmdList.foreach{println}

    println("-----------------------------------------------")
    val exp: Exp = Mul(Add(Number(5),Number(7)),Sub(Number(8),Number(2)))
    val instruk = CodeGenerator.ZwischenCode.genCode(exp)
    ZwischenCodePrinter.print(instruk)
  }
}
