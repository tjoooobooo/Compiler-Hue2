package mini_puck_c.frontend

import frontend.ProgSymbols.{ParamSymbol, ProcSymbol}

import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.input.Reader

/**
  * A TokenParser using a Lexer.
  * See scala.util.parsing.combinator.syntactical.TokenParsers for the general structure.
  *
  * The parser parses concrete syntax and produces an AST. It performs the following
  * semantic actions:
  *  - Identification of identifiers
  *    If the parser sees a definition, it generates
  *    a symbol for defined name. Subsequent usages of this name are identified with the name.
  *  - Insertion for dereference nodes
  *    If the parser sees a usage of a ref parameter it inserts a dereference node, to reflect the fact
  *    that the value is "behind a address"
  *
  * Identification of identifiers is supported by
  *   - env, the static environment, which handles compilation unit (CU) local definitions and usages,
  *     keeping track of scopes, and the
  *   - globalNameSpace, the global inter-CU namespace, which handles imports and exports of CUs.
  *
  * @param globalNameSpace represents the inter-CU namespace and provides symbols for imported names.
  */
class ProgParsers(globalNameSpace: GlobalNameSpace) extends TokenParsers {

  import ProgSymbols._

  // TokenParsers needs to know the type of the tokens
  override type Tokens = ProgTokens

  // TokenParsers needs to know the lexer
  override val lexical : ProgLexical = new ProgLexical

  // import definitions of the lexer (Token definitions, class Scanner)
  import lexical._

  // the static environment used to for identifier identification
  val env: StaticEnv = new EnvImpl



  // Parse numbers and identifiers -------------------------------------------------------------------------------------
  // (elem is defined in trait scala.util.parsing.combinator.Parsers)

  // parser for numbers
  private val num_any : Parser[Any] =
    elem("number", _.isInstanceOf[NumberToken])

  private def number: Parser[Number] =
    num_any   ^^ { case (x: Any) => Number(x.asInstanceOf[NumberToken].chars.toInt)}

  private val id_any : Parser[Any] =
    elem("identifier", _.isInstanceOf[IdentToken])

  // parser for identifiers
  private def ident: Parser[String] =
    id_any   ^^ { case (x: Any) => x.asInstanceOf[IdentToken].chars }


  // parse boolean expressions -----------------------------------------------------------------------------------------

  private def boolExp: Parser[BoolExp] = positioned {
    (arithExp <~ CompOpToken("<")) ~ arithExp ^^ { case e1 ~ e2 => Less(e1, e2) } |
      (arithExp <~ CompOpToken(">")) ~ arithExp ^^ { case e1 ~ e2 => Greater(e1, e2) } |
      (arithExp <~ CompOpToken("=")) ~ arithExp ^^ { case e1 ~ e2 => Equal(e1, e2) } |
      (arithExp <~ CompOpToken("<=")) ~ arithExp ^^ { case e1 ~ e2 => LessEq(e1, e2) } |
      (arithExp <~ CompOpToken(">=")) ~ arithExp ^^ { case e1 ~ e2 => GreaterEq(e1, e2) }
  }


  // parse expressions -------------------------------------------------------------------------------------------------

  private def arithExp: Parser[Exp] = positioned {
    chainl1(term, term, addOp)
  }

  private def addOp : Parser[(Exp, Exp) ⇒ Exp] =
    AddOpToken("+") ^^^ { (x: Exp, y: Exp) => Add(x, y) } |
    AddOpToken("-") ^^^ { (x: Exp, y: Exp) => Sub(x, y) }


  private def term = positioned { chainl1(factor, factor, multOp) }

  private def multOp : Parser[(Exp, Exp) ⇒ Exp] =
    MultOpToken("*") ^^^ { (x: Exp, y: Exp) => Mul(x, y) } |
    MultOpToken("/") ^^^ { (x: Exp, y: Exp) => Div(x, y) }


  private def factor: Parser[Exp] = positioned {
    number |
    LeftPToken("(") ~> arithExp <~ RightPToken(")") |
    locAccess
  }

  private def locAccess: Parser[LocAccess] = positioned {
    lExp ^^ { le => LocAccess(le) }
  }

  private def lExp: Parser[LocExp] = positioned {
    definedLoc ^^  {
      case symb@RefParamSymbol(_) => StarConv(DirectLoc(symb)) // Ref-Parameters need de-referencing
      case symb => DirectLoc(symb)
    }
  }

  // handling of defined names -----------------------------------------------------------------------------------------
  // All name look-up is done using  env, the static environment. Global (imported) names are entered into env.
  // Name look-up is performed in two stages:
  // - first check whether the name is defined at all (there is a symbol with this name in the actual environment)
  // - then whether it is of the expected kind

  // look-up name in env, assert that it is defined
  private def definedName: Parser[ProgSymbol] =
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
        for(param <- paramList)
          procsymb.params :+ param.symb
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
        case pSymb ~ te =>
          ValueParamDef(pSymb, te)
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

  private def cmd: Parser[Cmd] = positioned {
    (KwToken("IF") ~> boolExp <~ KwToken("THEN")) ~ rep(cmd) ~ (KwToken("ELSE") ~> rep(cmd) <~ KwToken("FI")) ^^ {
      case e ~ cthen ~ cElse => If(e, cthen, cElse)
    } |
      (KwToken("IF") ~> boolExp <~ KwToken("THEN")) ~ rep(cmd) <~ KwToken("FI") ^^ {
        case e ~ cthen => If(e, cthen, List())
      } |
      (KwToken("WHILE") ~> boolExp <~ KwToken("DO")) ~ rep(cmd) <~ KwToken("OD") ^^ {
        case e ~ cmdList => While(e, cmdList)
      } |
      (KwToken("WRITE") ~> LeftPToken("(") ~> arithExp) <~ RightPToken(")") <~ SemicolonToken(";") ^^ { case e => Write(e)
      } |
      definedProc ~ (LeftPToken("(") ~> repsep(arithExp, CommaToken(",")) <~ RightPToken(")")) <~ SemicolonToken(";") ^^ {
        case ps ~ args => Call(ps, args.map(Arg(_, None))) // method of parameter passing not yet known
      } |
      (lExp <~ AssignToken(":=")) ~ arithExp <~ SemicolonToken(";") ^^ {
        case ref ~ e => Assign(ref, e)
      }
  }

  // parse objects -----------------------------------------------------------------------------------------------------


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


  // parse -------------------------------------------------------------------------------------------------------------

  def parse(str: String) : Option[Obj] = {
    // remove trailing whitespaces before passing the input to the scanner
    val lexer: Reader[lexical.Token] = new lexical.Scanner(str.trim.stripSuffix(lexical.whitespacePattern))

    phrase(obj)(lexer) match { // phrase tries to analyse the whole text (the "phrase")
      case Success(tree,_)       => Some(tree)
      case error@NoSuccess(_,_)  =>
        println(error)
        None
    }
  }

}
