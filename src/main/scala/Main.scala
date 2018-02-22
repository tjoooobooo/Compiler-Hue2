object Main {

  def main(args: Array[String]): Unit = {
    var prog = ""
    do {
      println("Enter program, to stop hit Return")
      prog = scala.io.StdIn.readLine()
      if (prog != "") {
        printf("Hallo\n")
      }
    } while (prog != "")
  }

}
