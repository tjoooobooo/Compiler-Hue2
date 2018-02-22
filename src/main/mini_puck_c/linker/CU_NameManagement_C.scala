package mini_puck_c.linker

import mini_puck_c.separate_compilation.CU_NameManagement

/**
  * Manage and mange names (labels) in a way that conforms to the C-Code builder
  * which creates fragments of the C language.
  */
class CU_NameManagement_C extends CU_NameManagement {

  // name of the compilation unit
  private var cuName: String = ""

  // imported names
  private val importedNames: scala.collection.mutable.Map[String, Set[String]] = scala.collection.mutable.Map()

  // the set of exported names
  private val exportedNames: scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

  /**
    * Get the name of the compilation unit.
    * @return the name of the current compilation unit
    */
  override def getCuName: String= this.cuName

  /**
    * Set the name of the compilation unit.
    * @param cuName the name to be set
    */
  override def setCuName(cuName: String): Unit = this.cuName = cuName

  /**
    * Declare a name as exported
    * @param defName the exported name
    */
  override def addExport(defName: String): Unit =
    exportedNames += defName

  /**
    * Add a name to the set of imported names
    * @param defName the imported name
    */
  override def addImport(cuName: String, defName: String): Unit =
    importedNames += (cuName -> (importedNames.getOrElse(cuName, Set()) + defName) )

  /**
    * Create a global name.
    * Transform a local name to a globally valid name.
    * All local and exported names get the CU name as prefix,
    * imported names are prefixed with the name of the exporting CU.
    * @param name
    * @return a globally version of the name
    */
  override def toGlobal(name: String): String = {
    val cu = importedNames.filter {
      case (cuN, cuNames) => cuNames.contains(name)
    }
    if (cu.isEmpty) { // name is not imported: prefix with name of current CU
      cuName + "__" + name
    } else { // name is imported from some cu
      cu.keySet.last + "__" + name
    }
  }

  /**
    * Create a local name.
    * Transform a local name to a relocatable local name.
    * @param name
    * @return a relocatebale Name
    */
  override def toLocal(name: String): String =
    toGlobal("L"+name)


  override def getDependencies: List[String] =
    importedNames.keySet.toList

}
