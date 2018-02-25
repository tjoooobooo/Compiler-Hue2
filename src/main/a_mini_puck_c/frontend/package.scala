package mini_puck_c

/**
  * Contains the implementation of the compiler frontend: Parsing and context analysis.
  *
  * The main components are
  *  - Parsers: an implementation of a parser for Mini-Puck which produces an AST form the source and
  *  - Contextanalyser: an implementation of a context analyser which checks and transforms the AST.
  *
  * Parsers is a token parser which relies on a scanner component.
  *  - frontend.ProgTokens contains the defintion of all tokens
  *  - frontend.ProgLexical contains the scanner.
  *
  * CU-local management of names and symbols is done by
  *  - StaticEnv: a trait as interface to CU-local name and symbol management that deals with scopes, and
  *  - EnvImpl: a imperative implementation of StaticEnv
  *
  */
package object frontend {

}
