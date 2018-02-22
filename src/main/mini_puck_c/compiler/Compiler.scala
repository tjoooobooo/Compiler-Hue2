package mini_puck_c.compiler

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import mini_puck_c.backend.{C_CodeBuilder, CodeGenerator}
import mini_puck_c.frontend.Frontend
import mini_puck_c.separate_compilation.InMemoryGlobalNamespace

import scala.language.postfixOps

/**
  * This component is an implementation of the Mini-Puck compiler.
  *
  * It generates fragments of C-code as object files. The linker then may
  * link them to runnable C-code.
  */
object Compiler {

  val usage = "Usage: minipuckc -op objectPath filename"

  def main(args: Array[String]): Unit = {

    if (args.length != 3 || args(0) != "-op") {
      println(usage)
      return
    }

    /*
     * The objectPath (the folder) where the imported objects are to be found.
     */
    val objectPath: Path = Paths.get(s"${args(1)}").normalize().toAbsolutePath()

    if (!Files.isDirectory(objectPath)) {
      println(s"${args(2)} is not a directory")
      return
    }

    /*
     * Create a global name-space that represents the object-path
     * and create a front-end that operates on this name-space.
     */
    val globalNameSpace = new InMemoryGlobalNamespace(objectPath)
    val frontEnd = new Frontend(globalNameSpace)

    val fileIn: Path = objectPath.resolve(args(2))

    if (!Files.isRegularFile(fileIn)) {
      println(s"${args(2)} is not a a file in ${args(1)}")
      return
    }

    if (!Files.isReadable(fileIn)) {
      println(s"${args(2)} does not denote a readable file")
      return
    }

    frontEnd.analyse(fileIn) match {
      case Some(obj) =>

        val code = CodeGenerator.With(new C_CodeBuilder).genCodeFor(obj)

        val a: Array[String] = fileIn.toString().split("\\.")
        val fileNameWithoutExt: String =
          if (a.length >= 2) a(a.length-2) else fileIn.toString()

        // object code for a compilation unit
        val destPathObj = objectPath.resolve(fileNameWithoutExt + ".obj")
        Files.deleteIfExists(destPathObj)
        Files.write(destPathObj, code.getBytes(), StandardOpenOption.CREATE)

      case None =>
        println(s"Frontend failed on input")
    }
  }

}
