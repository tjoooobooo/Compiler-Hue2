package frontend

import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.input.Reader
import frontend.AST._
import ProgSymbols.ProgSymbol

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
    definedLoc ^^ {
      case symb@RefParamSymbol(_) => StarConv(DirectLoc(symb)) // Ref-Parameters need de-referencing
      case symb => DirectLoc(symb)
    }

  // handling of defined names -----------------------------------------------------------------------------------------
  // All name look-up is done using  env, the static environment. Global (imported) names are entered into env.
  // Name look-up is performed in two stages:
  // - first check whether the name is defined at all (there is a symbol with this name in the actual environment)
  // - then whether it is of the expected kind

  // look-up name in env, assert that it is defined
  private def definedName: ProgParsers.Parser[ProgSymbol] =
    ident ^? (
        env.lookup,
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
  // TODO PROGRAM
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
  // parameters and local definitions belong to inner scope

  private def procDefHeader: Parser[ProcSymbol] =
    KwToken("PROC") ~> ident ^? (
      env.defineProcedure.andThen( procSymbol => {env.enterScope(); procSymbol} ), // define proc in outer scope, then enter proc-scope
      { name => s"$name is already defined" }
    )

  private def varDefHeader: Parser[VarSymbol] =
    KwToken("VAR") ~> ident ^? (
      env.defineVariable,
      { name => s"$name is already defined" }
    )

  private def refParamDefHeader: Parser[RefParamSymbol] =
    KwToken("REF") ~> ident ^? (
      env.defineRefParam,
      { name => s"$name is already defined" }
    )

  private def valParamDefHeader: Parser[ParamSymbol] =
    ident ^? (
      env.defineValParam,
      { name => s"$name is already defined" }
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
      }| definedProc ~ (LeftPToken("(") ~> repsep(arithExp, CommaToken(",")) <~ RightPToken(")")) <~ SemicolonToken(";") ^^ {
        case ps ~ args => Call(ps, args.map(Arg(_, None))) // method of parameter passing not yet known
      } |
      (lExp <~ AssignToken(":=")) ~ arithExp <~ SemicolonToken(";") ^^ {
        case ref ~ e => Assign(ref, e)
      }
  }
  // parse objects -----------------------------------------------------------------------------------------------------

/*
TODO import objekte .. globalNameSpace?
  // store imported symbol in static environment
  private def imPort: Parser[(String, ProgSymbol)] =
    KwToken("IMPORT") ~> (ident <~ DotToken(".")) ~ (ident <~ SemicolonToken(";")) ^? (
      { case objName ~ defName if globalNameSpace.importSymbol.isDefinedAt(objName, defName) =>
        val symb = globalNameSpace.importSymbol(objName, defName)
        (objName, env.define(defName, symb))
      },
      { case objName ~ defName => s" '$objName . $defName'  can not be imported" }
    )

  private def imports: Parser[List[(String, ProgSymbol)]] =
    rep(imPort)

  private def exports: Parser[List[String]] =
    rep(exPort)

  private def exPort: Parser[String] =
    KwToken("EXPORT") ~> ident <~ SemicolonToken(";")

  private def obj: Parser[Obj] =
    objStart ~ (imports ~ exports ~ body) ^? ({
      case name ~ ((imPorts ~ exPorts) ~ Tuple2(defList, cmdList))
        if exPorts.foldLeft(true)((acc:Boolean, n:String) => // check whether exported names are defined
          acc && env.lookup.isDefinedAt(n)
        )  => {
        // put exported names form local environment to the global name-space
        exPorts.foreach( exportedName =>
          globalNameSpace.exportSymbol(name, exportedName, env.lookup(exportedName))
        )
        Obj(
          name,
          imPorts,
          exPorts,
          defList,
          cmdList)
      }

    }, {
      case name ~ ((imp ~ exp) ~ Tuple2(defList, cmdList)) => "some exported name is undefined"
    })

  private def objStart: Parser[String] =
    KwToken("OBJECT") ~> ident ^^ { x => env.enterScope(); x }

  private def body: Parser[(List[Definition], List[Cmd])] =
    rep(definition) ~ (KwToken("INIT") ~> rep(cmd) <~ KwToken("END")) ^^ { case defs ~ cmds =>
      (defs, cmds)
    }

*/

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





























