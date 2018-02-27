package slides_10.frontend

import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.input.Reader
import a_slides_10.frontend.AST._
import a_slides_10.frontend.{EnvImpl, ProgLexical, StaticEnv}

/*
 * A TokenParser using a Lexer.
 * See scala.util.parsing.combinator.syntactical.TokenParsers.
 */
object ProgParsers extends TokenParsers {

  import ProgSymbols._

  // the static environment used to for identifier identification
  val env: StaticEnv = new EnvImpl

  // TokenParsers needs to know the type of the tokens
  override type Tokens = ProgTokens

  // TokenParsers needs to know the lexer
  override val lexical : ProgLexical = new ProgLexical

  // import definitions of the lexer (Token definitions, class Scanner)
  import lexical._



  // Parse numbers and identifiers -------------------------------------------------------------------------------------
  // (elem is defined in trait scala.util.parsing.combinator.Parsers)

  // parser for numbers
  val num_any : Parser[Any] =
    elem("number", _.isInstanceOf[NumberToken])

  def number: Parser[Number] =
    num_any   ^^ { case (x: Any) => Number(x.asInstanceOf[NumberToken].chars.toInt)}

  // parser for identifiers
  val id_any : Parser[Any] =
    elem("identifier", _.isInstanceOf[IdentToken])

  def ident: Parser[String] =
    id_any   ^^ { case (x: Any) => x.asInstanceOf[IdentToken].chars }


  // parse boolean expressions -----------------------------------------------------------------------------------------

  def boolExp: Parser[BoolExp] =
    (arithExp <~ CompOpToken("<"))  ~ arithExp ^^ { case e1 ~ e2 => Less(e1, e2) } |
    (arithExp <~ CompOpToken(">"))  ~ arithExp ^^ { case e1 ~ e2 => Greater(e1, e2) } |
    (arithExp <~ CompOpToken("="))  ~ arithExp ^^ { case e1 ~ e2 => Equal(e1, e2) } |
    (arithExp <~ CompOpToken("<=")) ~ arithExp ^^ { case e1 ~ e2 => LessEq(e1, e2) } |
    (arithExp <~ CompOpToken(">=")) ~ arithExp ^^ { case e1 ~ e2 => GreaterEq(e1, e2) }



  // parse arithmetic expressions --------------------------------------------------------------------------------------

  def arithExp: Parser[ArithExp] = chainl1(term, term, addOp())

  def addOp() : Parser[(ArithExp, ArithExp) ⇒ ArithExp] =
    AddOpToken("+") ^^^ {(x:ArithExp, y: ArithExp) => Add(x,y)} |
    AddOpToken("-") ^^^ {(x:ArithExp, y: ArithExp) => Sub(x,y)}

  def term: ProgParsers.Parser[ArithExp] = chainl1(factor, factor, multOp)

  def multOp : Parser[(ArithExp, ArithExp) ⇒ ArithExp] =
    MultOpToken("*") ^^^ {(x:ArithExp, y: ArithExp) => Mul(x,y)}  |
    MultOpToken("/") ^^^ {(x:ArithExp, y: ArithExp) => Div(x,y)}

  def factor: Parser[ArithExp] =
    number   |
    LeftPToken("(") ~> arithExp <~ RightPToken(")") |
    refExp

  def refExp: Parser[LocAccess] = positioned {
    lExp ^^ {le => LocAccess(le) }
  }

  def lExp: Parser[RefExp] =
    ident ^?  ({  // check whether ident is a defined variable
      case name if (
       env.lookup(name) match {
        case success => true
        case failure => false
      })
       => VarRef2(env.lookup(name).asInstanceOf[VarSymbol]) // ident is a defined variable create AST-node using its definition
    },
      name => s"undefined name '$name'" // ident is not a defined variable
    )

  // handling of defined names -----------------------------------------------------------------------------------------
  // All name look-up is done using  env, the static environment. Global (imported) names are entered into env.
  // Name look-up is performed in two stages:
  // - first check whether the name is defined at all (there is a symbol with this name in the actual environment)
  // - then whether it is of the expected kind

  // look-up name in env, assert that it is defined
  private def definedName: ProgParsers.Parser[VarRef2] =
    ident ^? ({
      case name if (
        env.lookup(name) match {
          case succes => true
          case failure => false
        })
      => VarRef2(env.lookup(name).asInstanceOf[VarSymbol])
    },
        name => s"Name '$name' is undefined"
    )

  // assert that symbol associated with a name denotes a location
  private def definedLoc: Parser[LocSymbol] =
    definedName ^? (
      { case symb: LocSymbol => symb },
      name => s"Name '$name' is not a location"
    )

  // assert that symbol associated with a name denotes a procedure
  private def definedProc: Parser[ProcSymbol] =
    definedName ^? (
      { case symb: ProcSymbol => symb },
      name => s"Name '$name' is not a procedure"
    )

  // parse programs ----------------------------------------------------------------------------------------------------
  def prog: Parser[Prog] =
    progStart ~> body ^^ { case (defList, cmdList) => Prog(defList, cmdList) }

  // enter scope when keyword PROGRAM appears
  def progStart: Parser[Any] =
    KwToken("PROGRAM") ^^ { x => env.enterScope(); x }

  // leave scope after parsing the body
  def body: Parser[(List[Definition], List[Cmd])] =
    rep(definition) ~ (KwToken("BEGIN") ~> rep(cmd) <~ KwToken("END")) ^^ { case defs ~ cmds =>
      env.leaveScope()
      (defs, cmds)
    }
  // parse type expressions --------------------------------------------------------------------------------------------

  private def typeExp: Parser[TypeExp] =
    KwToken("INT")    ^^^ IntTypeExp



  // parse definitions -------------------------------------------------------------------------------------------------
  // when parsing a definition, the defined name is entered into the static environment, which creates a symbol for
  // the name

  private def varDef: Parser[VarDef] = positioned {
    (varDefHeader <~ ColonToken(":")) ~ typeExp ~ (AssignToken(":=") ~> arithExp <~ SemicolonToken(";")) ^^ {
      case varsymb ~ t ~ e => VarDef(varsymb, t, e)
    }
  }

  private def procDef: Parser[ProcDef] = positioned {
    procDefHeader ~ (LeftPToken("(") ~> repsep(paramDef, CommaToken(",")) <~ RightPToken(")")) ~ rep(varDef) ~ (KwToken("BEGIN") ~> rep(cmd) <~ KwToken("END"))  ^^ {
      case procsymb ~  paramList ~ vardefs ~ cmds =>
        env.leaveScope() // leave scope of procedure (scope was entered when parsing the procedure name)
        ProcDef(procsymb, paramList, vardefs, cmds)
    }
  }

  private def definition: Parser[Definition] = positioned {
    varDef |
      procDef
  }

  private def paramDef: Parser[ParamDef] =
    (refParamDefHeader <~ ColonToken(":")) ~ typeExp ^^ {
      case pSymb ~ te => RefParamDef(pSymb, te)
    } |
      (valParamDefHeader <~ ColonToken(":")) ~ typeExp ^^ {
        case pSymb ~ te => ValueParamDef(pSymb, te)
      }


  // parse start of definitions ----------------------------------------------------------------------------------------
  // the nane that is introduced with a definition has to be entered into the static environment.
  // Parsing of definition headers is separated form parsing of the rest in oder to assure that the defined name
  // is put into the static environment before parsing the rest of the definition.

  // parse first part (essentially the name) of a procedure definition
  // name of procedure belongs to outer scope
  // parameters and local definitions belong to inner scopre
  private def procDefHeader: Parser[ProcSymbol] =
    KwToken("PROC") ~> ident ^? (
      env.defineProcedure.andThen( procSymbol => {env.enterScope(); procSymbol} ), // define proc in outer scope, then enter proc-scope
      { case name => s"$name is already defined" }
    )

  private def varDefHeader: Parser[VarSymbol] =
    KwToken("VAR") ~> ident ^? (
      env.defineVariable,
      { case name => s"$name is already defined" }
    )

  private def refParamDefHeader: Parser[RefParamSymbol] =
    KwToken("REF") ~> ident ^? (
      env.defineRefParam,
      { case name => s"$name is already defined" }
    )

  private def valParamDefHeader: Parser[ParamSymbol] =
    ident ^? (
      env.defineValParam,
      { case name => s"$name is already defined" }
    )



  // parse commands ----------------------------------------------------------------------------------------------------
  def cmd: Parser[Cmd] = positioned {
    (KwToken("IF") ~> boolExp <~ KwToken("THEN")) ~ rep(cmd) ~ (KwToken("ELSE") ~> rep(cmd) <~ KwToken("FI")) ^^ {
      case e ~ cthen ~ cElse => If(e, cthen, cElse)
    } |
      (KwToken("IF") ~> boolExp <~ KwToken("THEN")) ~ rep(cmd) <~ KwToken("FI") ^^ {
        case e ~ cthen => If(e, cthen, List())
      } |
      (KwToken("WHILE") ~> boolExp <~ KwToken("DO")) ~ rep(cmd) <~ KwToken("OD") ^^ {
        case e ~ cmdList => While(e, cmdList)
      } |
      (KwToken("WRITE") ~> LeftPToken("(") ~> arithExp) <~ RightPToken(")") <~ SemicolonToken(";") ^^ (e => Write(e)) |
      (lExp <~ AssignToken(":=")) ~ arithExp <~ SemicolonToken(";") ^^ {
        case ref ~ e => Assign(ref, e)
      }
  }

  def parse(str: String): Prog = {

    // remove trailing whitespaces before passing the input to the scanner
    val lexer: Reader[lexical.Token] = new lexical.Scanner(str.trim.stripSuffix(lexical.whitespacePattern))

    phrase(prog)(lexer) match { // phrase tries to analyse the whole text (the "phrase")
      case Success(tree,_)       => tree
      case error@NoSuccess(_,_)  => println(error)
        throw new IllegalArgumentException("Parser Error")
    }
  }

}





























