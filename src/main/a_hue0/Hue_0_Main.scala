package thm.mni.cb1718.hue_0

import scala.util.Try

/**
  * Compilerbau 2017 / 2018 Hausuebung 0
  * Gruppe: Thomas Schwabauer, Muhammed Dersüneli
  */
object Hue_0_Main {

  def main(args: Array[String]): Unit =a {

    var sillyInput = true

    do {
      println("To run the compiler enter 1, to run the interpreter enter 2")
      try {
        val choice: Int = scala.io.StdIn.readInt()
        choice match {
          case 1 =>
            sillyInput = false
            compiler.IDE.run()
          case 2 =>
            sillyInput = false
            interpreter.Interpreter.run()
          case _ =>
        }
      }catch {
          case _: NumberFormatException =>
        }
      } while (sillyInput)
  }
}
