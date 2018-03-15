
import java.io.PrintWriter

import CodeGenerator.AbstractSyntaxPrinter
import frontend.{ContextAnalysis, ProgParsers}

import scala.util.Try

object Main {
  //TODO wir brauchen noch irgendeine Main

  def main(args: Array[String]): Unit = {
    var pfad = "PuckTest//puck//"
    var puckFile = scala.io.Source.fromFile(pfad + "test.puck").mkString
    //TODO proc aufruf was mit ref da
    var parsed = Try(ProgParsers.parse(puckFile))
    if(parsed.isSuccess) {
      var analysed = Try(ContextAnalysis.checkContext(parsed.get))
      if(analysed.isSuccess){
        var zwischenCode = ZwischenCode.ZwischenCodeGenerator.translate(analysed.get)
        var assembler = CodeGenerator.GenAssemblerLines.gen(zwischenCode)
        new PrintWriter("PuckTest//TestDateien//Test.a") {
          write(AbstractSyntaxPrinter.apply(assembler))
          close()
        }
      } else println(analysed.get)

    }

  }

}
