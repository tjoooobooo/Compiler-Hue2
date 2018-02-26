import slides_10.frontend.ProgParsers

object Main {
import a_slides_10._
  def main(args: Array[String]): Unit = {
    var prog = "PROGRAM BEGIN  END"
   var parsed = ProgParsers.parse(prog)
    println(parsed)
  }

}
