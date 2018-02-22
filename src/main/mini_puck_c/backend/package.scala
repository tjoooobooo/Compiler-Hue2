package mini_puck_c

/**
  * Contains the implementation of a backend. The backend transforms the analysed AST produced by the frontend
  * to linkable object code.
  *
  * The main components are
  *   - CodeGenerator: knows how to transform an AST into sequential code. It relies on
  *     a code builder to actually generate machine-specific code.
  *
  *   - CodeBuilder: is an interface to a component that kows how generate machine-specific code given
  *     some abstract intermediate code instruction.
  *
  *   - C_CodeBuilder: knows how ton generate C-code form an intermediate instruction.
  *
  *   - IMCodeBuilder: just generate intermediate code in textual form form intermediate code.
  *     It may be used for debugging purposes.
  *
  *   - RuntimeOranisation encapsulates definitions and constants related to runtime issues (e.g. stack frame format)
  *     that are used by the context analyser.
  *
  */
package object backend {

}
