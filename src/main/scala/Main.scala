
import frontend.AST.{Number, VarDef, Write}
import frontend.ProgParsers

object Main {

  def main(args: Array[String]): Unit = {
    var pfad = "PuckTest//"
    var puckFile = scala.io.Source.fromFile(pfad + "proc_aufruf.puck").mkString
    var parsed = ProgParsers.parse(puckFile)
    println(parsed)
  }
}
