package mini_puck_c.separate_compilation

import java.nio.file.Path

import mini_puck_c.frontend.Frontend
import mini_puck_c.frontend.ProgSymbols.ProgSymbol

/**
  * Instances of this class represent the global name-space, as seen by the compilation of one object.
  * This name-space consists of all names that relevant for a compilation of the object,
  * i.e. names that are imported or exported by the object or any of the objects that are used in imports.
  *
  * The global name-space is created as a map [objectName -> [symbolName -> symbol]] recursively
  * when analysing an object.
  */
class InMemoryGlobalNamespace(folder: Path) extends GlobalNameSpace {

  globalNamespace =>

  // avoid compilation cycles
  var cycleControl: List[String] = List()

  // the global name-space: object-name -> (symbol-name -> symbol)
  // objects have to be unique in the global name-space
  // each object has an associated set of exported symbols
  private var nameSpaceCache: Map[String, Map[String, ProgSymbol]] = Map()

  /**
    * Import a symbol. I.e. look it up (with object-name, symbol-name) in the global namespace.
    * If the object-name is not to found in the nameSpaceCache then the object is not jet analysed: analyse it
    * and then check again.
    */
  override val importSymbol: PartialFunction[(String, String), ProgSymbol] = new PartialFunction[(String, String), ProgSymbol] {

    override def isDefinedAt(obj_name: (String, String)): Boolean = obj_name match {

      case (objName: String, name: String) =>

        nameSpaceCache.get(objName) match {

          case None =>
            if (cycleControl.contains(objName)) {
              println("cyclic import")
              return false
            }

            cycleControl = objName :: cycleControl

            // lookup of a symbol from a not jet compiled source: Analyse object
            val referencedObj: Path = folder.resolve(objName+".puck")
            Frontend(globalNamespace).analyse(referencedObj)

            // hopefully, the symbol is now defined as a side effect of analysing the object
            // from which the name was imported

            nameSpaceCache.get(objName) match {
              case None => false
              case Some(objDefs) =>
                objDefs.get(name) match {
                  case None => false
                  case Some(_) => true
              }
            }

          case Some(objDefs) =>
            objDefs.get(name) match {
              case None => false
              case Some(_) => true
            }
        }
    }

    override def apply(obj_name: (String, String)): ProgSymbol = obj_name match {
      case (objName, name) if isDefinedAt(objName, name) =>
        nameSpaceCache(objName)(name)
    }

  }

  override val exportSymbol: PartialFunction[(String, String, ProgSymbol), Unit] = {
    case (objectName, symbolName, symbol)
      if ! (nameSpaceCache.isDefinedAt(objectName) && nameSpaceCache(objectName).isDefinedAt(symbolName)) =>
      nameSpaceCache = nameSpaceCache + (objectName -> (nameSpaceCache.getOrElse(objectName, Map()) + (symbolName -> symbol) ))
  }

}
