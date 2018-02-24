object Main {

  def main(args: Array[String]): Unit = {
    var prog = "3+3"
    val s = Scanner.apply(prog)
    println(s)
    Parser.parse(s)
  }

}
