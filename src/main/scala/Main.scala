import frontend.ProgParsers

object Main {

  def main(args: Array[String]): Unit = {
    var prog = "PROGRAM VAR Z : INT := 2; PROC test(a: INT) BEGIN Z := 3; END BEGIN END"
   var parsed = ProgParsers.parse(prog)
    println(parsed)
  }

}
