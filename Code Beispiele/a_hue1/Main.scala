/*
* Muhammed Dersüneli 5134638
* Thomas Schwabauer 5135080
 */
package puck.assembler

import puck.common.LinedMessageLogger

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Command line arguments invalid")
      return
    }

    val success = Assembler(Assembler.CompilerOptions(
      args(0),
      args(0).replaceFirst("[.][^.]+$", ".o")
    ))

    println(LinedMessageLogger)

    if (success) {
      println("Compilation succeeded")
    } else {
      println("Compilation failed")
    }
  }
}
