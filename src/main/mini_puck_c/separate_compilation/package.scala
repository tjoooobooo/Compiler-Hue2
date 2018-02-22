package mini_puck_c

/**
  * Contains the base traits and classes that deal with separate compilation:
  *
  * - GlobalNameSpace: This is the main trait of this package.
  *   This trait defines an interface of the compiler to the global namespace,
  *   which is seen as a persistent inter-CU repository of symbols.
  *   The compiler processes an CU but also needs access to global names.
  *   The global namespace contains names that are exported by some CU and that
  *   may be imported by the CU imports that the compiler currently is compiling.
  *   It accepts names that are exported by the CU that is currently compiled.
  *
  * - InMemoryGlobalNamespace: is a simple implementation of GlobalNameSpace.
  *   It does not persist any global symbol, but just analyses (using
  *   [[mini_puck_c.frontend.Frontend]]) CUs mentioned in import statements,
  *   and caches their exported symbols.
  *
  * - Trait CU_NameManagement  is the type of an interface
  *   of a [[mini_puck_c.backend.CodeBuilder]] to a component that transforms (mangles) labels (names)
  *   to a form that is needed by the the linker.
  */
package object separate_compilation {

}
