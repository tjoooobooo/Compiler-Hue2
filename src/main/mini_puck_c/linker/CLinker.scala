package mini_puck_c.linker

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.io.Source

/**
  * The linker links "object files" containg C-code to an "executable" C-program.
  */
object CLinker extends Linker {

  /**
    * Instances of this class do the actual linking by following import statements.
    * They collect code and the names of the imported objects (CUs).
    */
  private case class ImportResolver(objectPath: Path) {

    private var cycleControl: List[Path] = List()
    private var imports: List[String] = List()
    private var objs: List[String] = List()
    private val strBuf: StringBuilder = new StringBuilder

    private val objPatS = """^\.OBJECT\s+(\w+)"""
    private val objPat = objPatS.r

    private val usePatS = """^\.USE\s+(\w+)"""
    private val usePat = usePatS.r

    /**
      * Follow imports and collect the code needed.
      * @param fileIn The file to start with.
      */
    def resolve(fileIn: Path): Unit = {

      if (!cycleControl.contains(fileIn)) {

        cycleControl = fileIn :: cycleControl

        val source = new String(Files.readAllBytes(fileIn), StandardCharsets.UTF_8)

        for (line <- Source.fromBytes(Files.readAllBytes(fileIn), "UTF-8").getLines) {

          line match {
            case usePat(objName) =>
              imports = objName :: imports
              val importName = objName + ".obj"
              val importPath = objectPath.resolve(importName)
              resolve(importPath)
            case objPat(objName) =>
              objs = objName :: objs
            case _ =>
              strBuf ++= line + "\n"
          }
        }

      }
    }

    /**
      * Get the code collected by a call of resolve.
      * @return the code that was collected
      */
    def getCode: String = strBuf.toString()

    /**
      * Get the name of all objects (CUs) from which something was imported.
      * @return the list of names of objects that were imported.
      */
    def getUsedObjs: List[String] = imports

    /**
      * Get the name of the object (CU) with which the resolving process started.
      * @return the name of the first object.
      */
    def getMainObj: String = objs.last

  }


  /**
    *
    * @param fileIn
    * @param folder     the folder where to search for used (imported) CUs
    * @param mainProc   the procedure to be activated when running the executbale
    * @return           the executable code produced.
    */
  override def link(fileIn: Path, folder: Path, mainProc: String) : String = {

    // recursively collect the code of all object that are used
    val resolver = ImportResolver(folder)
    resolver.resolve(fileIn)
    val usedObjs: List[String] = resolver.getUsedObjs

    val strBuf: StringBuilder = new StringBuilder

    // predude
    strBuf ++=
      """
#include <stdio.h>
#include <stdlib.h>

typedef union Cell *CellRef;
typedef void *CodeRef;

// a cell in memory may contain all kinds of values.
typedef union Cell {
        int        value;    // cell contains value
        CellRef    cellRef;  // cell contains pointer to cell
        CodeRef    codeRef;  // cell contains pointer to code
} Cell;

// temporaries for values and addresses
Cell T_0, T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9; // value temporaries
Cell A_0, A_1, A_2, A_3, A_4, A_5, A_6, A_7, A_8, A_9; // address temporaries

Cell staticData[100]; // static data area

// "Register" that contains the (last) address of the static data area
Cell SDA;

// the runtime call stack
Cell frameStack[1000];

Cell FP;      // Frame pointer
Cell SP;      // Stack pointer
CodeRef RR;   // return register

int main(void) {
"""
    strBuf ++= "\tSDA.cellRef = &staticData[100];\n"
    strBuf ++= "\tSP.cellRef = &(frameStack[999]);\n"

    // generate calls of initialization code
    usedObjs.foreach(objName => {
      strBuf ++= s"\t//call initializer for object $objName \n"
      strBuf ++= "\t(SP.cellRef)->cellRef = FP.cellRef; SP.cellRef--;\n"
      strBuf ++= s"\t(SP.cellRef)->codeRef = &&${objName}__AFTER_INIT; SP.cellRef = SP.cellRef-1;\n"
      strBuf ++= s"\tgoto ${objName}__OBJ_INIT_PROC___;\n"
      strBuf ++= s"${objName}__AFTER_INIT:;\n"
      strBuf ++= "\tSP.cellRef = SP.cellRef+0;\n"
      strBuf ++= "\t(SP.cellRef)++; FP.cellRef = (SP.cellRef)->cellRef;\n"
      strBuf ++= "\n"
    })

    val objectName: String = resolver.getMainObj

    // generate call of initialization code of main object
    strBuf ++= s"\t//call initializer for main object $objectName \n"
    strBuf ++= "\t(SP.cellRef)->cellRef = FP.cellRef; SP.cellRef--;\n"
    strBuf ++= s"\t(SP.cellRef)->codeRef = &&${objectName}__AFTER_INIT; SP.cellRef = SP.cellRef-1;\n"
    strBuf ++= s"\tgoto ${objectName}__OBJ_INIT_PROC___;\n"
    strBuf ++= s"${objectName}__AFTER_INIT:;\n"
    strBuf ++= "\tSP.cellRef = SP.cellRef+0;\n"
    strBuf ++= "\t(SP.cellRef)++; FP.cellRef = (SP.cellRef)->cellRef;\n"
    strBuf ++= "\n"


    // add call of main procedure

    strBuf ++= "\n"
    strBuf ++= s"\t// call proc $mainProc of object $objectName"
    strBuf ++= s"\t(SP.cellRef)->cellRef = FP.cellRef; SP.cellRef--;\n"
    strBuf ++= s"\t(SP.cellRef)->codeRef = &&AFTER_MAIN; SP.cellRef = SP.cellRef-1;\n"
    strBuf ++= s"\tgoto ${objectName}__${mainProc};\n"
    strBuf ++= s"\tAFTER_MAIN:;\n"
    strBuf ++= s"\tSP.cellRef = SP.cellRef+0;\n"
    strBuf ++= s"\t(SP.cellRef)++; FP.cellRef = (SP.cellRef)->cellRef;\n"
    strBuf ++= s"\texit(0);\n"
    strBuf ++= s"\n"

    // add generated code of the objects
    strBuf ++= s"\t// generated code\n"
    strBuf ++= resolver.getCode
    strBuf ++= "}\n"

    strBuf.toString()

  }
}
