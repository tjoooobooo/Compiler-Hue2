package thm.mni.cb1718.hue_0.compiler

object IDE {

  def run(): Unit = {
    var prog = ""
    do {
      println("Enter program, to stop hit Return")
      prog = scala.io.StdIn.readLine()
      if (prog != "") {
        val code = Compiler.compile(prog)
        printf("%s\n", code)
        Machine.reset
        Machine.load(code)
        Machine.run

      }
    } while (prog != "")
  }
}
