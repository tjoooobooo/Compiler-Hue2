/* Teammitglieder:
 * Muhammed Dersüneli
 * Thomas Schwabauer
 */
import java.io.PrintWriter

import CodeGenerator.AbstractSyntaxPrinter
import frontend.{ContextAnalysis, ProgParsers}

import scala.util.Try

object Main {

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Command line arguments invalid")
      return
    }
    //var pfad = "PuckTest//puck//"
    //var puckFile = scala.io.Source.fromFile(pfad + "test.puck").mkString
    var puckFile = scala.io.Source.fromFile(args(0)).mkString
    var parsed = Try(ProgParsers.parse(puckFile))
    if(parsed.isSuccess) {
      var analysed = Try(ContextAnalysis.checkContext(parsed.get))
      if(analysed.isSuccess){
        var zwischenCode = ZwischenCode.ZwischenCodeGenerator.translate(analysed.get)
        var assembler = CodeGenerator.GenAssemblerLines.gen(zwischenCode)
        new PrintWriter(args(0).replaceFirst("[.][^.]+$", ".o")) {
          //"PuckTest//TestDateien//Test.a"
          write(AbstractSyntaxPrinter.apply(assembler))
          close()
        }
      } else println(analysed.get)

    }

  }

}
