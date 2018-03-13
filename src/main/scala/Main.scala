
import java.io.PrintWriter

import CodeGenerator.AbstractSyntaxPrinter
import frontend.ProgParsers

object Main {
  //TODO wir brauchen noch irgendeine Main

  def main(args: Array[String]): Unit = {
    var pfad = "PuckTest//puck//"
    var puckFile = scala.io.Source.fromFile(pfad + "test.puck").mkString
    //TODO proc aufruf was mit ref da
    var parsed = ProgParsers.parse(puckFile)
    var res = ZwischenCode.ZwischenCodeGenerator.translate(parsed)
    var syntax = CodeGenerator.GenAssemblerLines.gen(res)
    new PrintWriter("PuckTest//TestDateien//Test.a") {
      write(AbstractSyntaxPrinter.apply(syntax))
      close()
    }
  }

}
