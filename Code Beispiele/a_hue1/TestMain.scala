package puck.assembler

object TestMain {
  def main(args: Array[String]): Unit ={
    var tests = List[String]("AllInstructions","ArrayTest","ControlStructureTest","echo","FunctionTest","MinimalExample","OperatorTest","StdIO")
    for(test <- tests) {
      var ar = Array("tst\\"+test+".a")
      Main.main(ar)
    }
  }
}
