package mini_puck_c.frontend

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import mini_puck_c.compiler.AST.Obj
import mini_puck_c.separate_compilation.GlobalNameSpace

import scala.util.{Failure, Success}

/**
  * Instances of this class form a compiler front end that operates on a global name space.
  * They are created when  compilation unit ist to analysed.
  *
  * The global name spaces provides all symbols that are imported by the compilation unit and
  * accepts all exports defined in the compilation unit.
  *
  * @param globalNameSpace  The global ame space: where to import symbols from and export symbols to.
  */
case class Frontend(globalNameSpace: GlobalNameSpace) {

  /**
    * Analyse a source: check it, create AST, store exported defintiions,
    * and put all exported symbols into the global namespace.
    *
    * @param file   the path of the source file, a compilation unit (CU)
    * @return       the ast of the analysed CU
    */
  def analyse(file: Path): Option[Obj] = {

    val source = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)

    val treeOpt: Option[Obj] = new ProgParsers(globalNameSpace).parse(source.stripMargin)

    treeOpt match {

      case None =>
        println("Parsing failed, compilation aborted")
        None

      case Some(obj) =>
        ContextAnalyser.analyseObj(obj) match {
          case Success(_) =>
            Some(obj)

          case Failure(e) =>
            println(s"Context analyser complains: $e")
            None
        }


        Some(obj)

    }
  }
}
