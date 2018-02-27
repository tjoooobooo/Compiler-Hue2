import slides_10.frontend.ProgParsers

object Main {
import a_slides_10._
  def main(args: Array[String]): Unit = {
    var prog = "PROGRAM VAR Z:= 2; BEGIN WRITE(Z); END"
   var parsed = ProgParsers.parse(prog)
    println(parsed)
  }

}
