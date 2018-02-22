package mini_puck_c.separate_compilation

/**
  * Instances of this trait encapsulate naming issues that arise because of separate compilation.
  * They are used to create and manage names (labels) according to the demands of the
  * backend and its separate compilation regime.
  *
  * CU_NameManagement objects are created by a [[mini_puck_c.backend.CodeBuilder]]
  * per compilation unit (CU) and transform names (labels) of a
  * this CU to the form in which they have to appear in a linkable unit.
  */
trait CU_NameManagement {

  /**
    * Set the name of the compilation unit.
    * @param cuName the name to be set
    */
  def setCuName(cuName: String): Unit

  /**
    * Get the name of the compilation unit.
    * @return the name to be set
    */
  def getCuName: String


  /**
    * Declare a name as exported.
    * @param defName the exported name
    */
  def addExport(defName: String): Unit

  /**
    * Add a name to the set of imported names
    * @param defName the imported name
    */
  def addImport(cuName: String, defName: String): Unit

  /**
    * Create a global name. A global name is valid across all CUs.
    * Transform a local name to a globally valid name.
    * All local and exported names get the CU name as prefix,
    * imported names are prefixed with the name of the exporting CU.
    * @param name
    * @return a globally version of the name
    */
  def toGlobal(name: String): String

  /**
    * Create a local name. A local name is valid only in one (this) CU.
    * Transform a local name to a relocatable local name.
    * @param name
    * @return a relocatebale Name
    */
  def toLocal(name: String): String

  /**
    * Get the names of all CUs that this CU depends on.
    * @return list of CU names which export names that are imported by this CU.
    */
  def getDependencies: List[String]

}
