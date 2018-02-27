package mini_puck_c.linker

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

/**
  * Base class of linkers.
  *
  * A linker generates executable code form object code.
  *
  * Object code contains references to names (labels) defined elsewhere in other files
  * A linker puts together an object file and all object files it depends on.
  * All inter-file references are resolved.
  */
abstract class Linker {

  /**
    * Build an executable file by linking a file containing a compiled compilation unit
    * with all CUs it depends on.
    *
    * @param file       the file that contains the object code of the CU to be linked
    * @param folder     the folder where to search for used (imported) CUs
    * @param mainProc   the procedure to be activated when running the executbale
    * @return           the executable code produced.
    */
  protected def link(file: Path, folder: Path, mainProc: String): String

  private val usage = "Usage: minipuckln -op objectPath -proc procName filename"


  def main(args: Array[String]): Unit = {
    if (args.length != 5 || args(0) != "-op" || args(2) != "-proc") {
      println(usage)
      return
    }

    /*
     * The objectPath (the folder) where the imported objects are to be found.
     */
    val objectPath: Path = Paths.get(s"${args(1)}").normalize().toAbsolutePath()

    println("objectPath: " + objectPath)

    if (!Files.isDirectory(objectPath)) {
      println(s"${args(1)} is not a directory")
      return
    }

    val fileName = args(4)

    val fileIn: Path = objectPath.resolve(fileName)

    println("fileIn: " + fileIn)

    if (!Files.isRegularFile(fileIn)) {
      println(s"$fileName is not a a file in $objectPath")
      return
    }

    if (!Files.isReadable(fileIn)) {
      println(s"${args(2)} does not denote a readable file")
      return
    }

    val a: Array[String] = fileIn.toString().split("\\.")
    val fileNameWithoutExt: String =
      if (a.length >= 2) a(a.length - 2) else fileIn.toString()

    val destPath = objectPath.resolve(fileNameWithoutExt + ".c")

    val mainProc = args(3)

    val executableCode: String = link(fileIn, objectPath, mainProc)

    Files.deleteIfExists(destPath)
    Files.write(destPath, executableCode.getBytes(), StandardOpenOption.CREATE)
  }
}
