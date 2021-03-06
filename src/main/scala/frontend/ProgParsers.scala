package frontend

import scala.util.parsing.input.Reader
import frontend.AST._
import ProgSymbols.ProgSymbol
import backend.RuntimeOrganisation
import backend.RuntimeOrganisation.RTLocInfo
import frontend.StaticTypes.IntTypeInfo

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.syntactical.TokenParsers

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
    (arithExp <~ CompOpToken("=="))  ~ arithExp ^^ { case e1 ~ e2 => Equal(e1, e2) } |
      (arithExp <~ CompOpToken("!="))  ~ arithExp ^^ { case e1 ~ e2 => NotEq(e1, e2) } |
    (arithExp <~ CompOpToken("<=")) ~ arithExp ^^ { case e1 ~ e2 => LessEq(e1, e2) } |
    (arithExp <~ CompOpToken(">=")) ~ arithExp ^^ { case e1 ~ e2 => GreaterEq(e1, e2) }

  // parse arithmetic expressions --------------------------------------------------------------------------------------

  def arithExp: Parser[Exp] = chainl1(term, term, addOp()|bitOp())

  def bitOp() : Parser[(Exp, Exp) ⇒ Exp] =
      BitwiseOpToken("&") ^^^ {(x:Exp, y: Exp) => And(x,y)} |
      BitwiseOpToken("|") ^^^ {(x:Exp, y: Exp) => Or(x,y)} |
      BitwiseOpToken("^") ^^^ {(x:Exp, y: Exp) => Xor(x,y)} |
      BitwiseOpToken("<<") ^^^ {(x:Exp, y: Exp) => Sl(x,y)} |
      BitwiseOpToken(">>") ^^^ {(x:Exp, y: Exp) => Sr(x,y)}


  def addOp() : Parser[(Exp, Exp) ⇒ Exp] =
    AddOpToken("+") ^^^ {(x:Exp, y: Exp) => Add(x,y)} |
    AddOpToken("-") ^^^ {(x:Exp, y: Exp) => Sub(x,y)}

  def term: ProgParsers.Parser[Exp] = chainl1(factor, factor, multOp)

  def multOp : Parser[(Exp, Exp) ⇒ Exp] =
      MultOpToken("*") ^^^ {(x:Exp, y: Exp) => Mul(x,y)}  |
      MultOpToken("/") ^^^ {(x:Exp, y: Exp) => Div(x,y)}  |
      MultOpToken("%") ^^^ {(x:Exp, y: Exp) => Mod(x,y)}

  def factor: Parser[Exp] =
    number   |
    LeftPToken("(") ~> arithExp <~ RightPToken(")") |
    locAccess

  def locAccess: Parser[LocAccess] = positioned {
    lExp ^^ {le => LocAccess(le) }
  }
  def lExp: Parser[LocExp] =
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
  var objectName: String = ""
  // parse programs ----------------------------------------------------------------------------------------------------
  def obj: Parser[Obj] =
    objStart ~> body ^^ { case (defList, cmdList) => Obj(objectName,None,None,defList, cmdList) }

  // enter scope when keyword PROGRAM appears
  def objStart: Parser[Any] =
    KwToken("object") ~> ident ^^ { x =>
      env.enterScope()
      objectName = x.toString
    }

  // leave scope after parsing the body
  def body: Parser[(List[Definition], List[Cmd])] =
    opt(rep(varDef)) ~ rep(definition) ~ (KwToken("init") ~> rep(cmd) <~ KwToken("end")) ^^ { case vars ~ defs ~ cmds =>
      env.leaveScope()
      var definitions : List[Definition] = vars.get.flatten ++ defs
      (definitions, cmds)
    }
  // parse type expressions --------------------------------------------------------------------------------------------

  private def typeExp: Parser[TypeExp] =
    KwToken("int")    ^^^ IntTypeExp



  // parse definitions -------------------------------------------------------------------------------------------------
  // when parsing a definition, the defined name is entered into the static environment, which creates a symbol for
  // the name

  private def varDef: Parser[List[VarDef]] = {
    var result: ListBuffer[VarDef] = new ListBuffer[VarDef]()
    varDefHeader ~ (opt(CommaToken(",") ~> rep(ident <~ opt(CommaToken(",")))) <~ ColonToken(":")) ~
      typeExp <~ SemicolonToken(";") ^^ {
      case varsymb ~ symbols ~ t  =>
        result += VarDef(varsymb, t, None)
        if (symbols.isDefined) {
          for (symb <- symbols.get) {
            var a = env.defineVariable(symb)
            a.staticType = Some(IntTypeInfo)
            result += VarDef(a, t, None)
          }
        }
        result.toList
    }
  }

  private def varDefHeader: Parser[VarSymbol] =
    KwToken("var") ~> ident ^? (
      env.defineVariable,
      { name => s"$name is already defined" }
    )

  private def procDef: Parser[ProcDef] = positioned {
    procDefHeader ~ (LeftPToken("(") ~> repsep(paramDef, SemicolonToken(";")) <~ RightPToken(")")) ~ rep(varDef) ~ (KwToken("begin") ~> rep(cmd) <~ KwToken("end"))  ^^ {
      case procsymb ~  paramList ~ vardefs ~ cmds =>
         // leave scope of procedure (scope was entered when parsing the procedure name)
        env.leaveScope()
        // add parameter to procsymbol
        var params = new ListBuffer[ParamSymbol]()
        for(p <- paramList) params += p.symb
        procsymb.params = Option(params.toList)
        // add local variables to procsymb
        var locals = new ListBuffer[VarSymbol]()
        for(vardefs2 <- vardefs) {
          for (elem <- vardefs2) {locals += elem.symb}
        }
        procsymb.locals = Option(locals.toList)
        RuntimeOrganisation.frameLayout(1,procsymb)
        /*flatten
         *List(List(1,2), List(3,4)).flatten
         *> List(1,2,3,4)
         */
        ProcDef(procsymb, paramList, vardefs.flatten, cmds)
    }
  }

  private def definition: Parser[Definition] =  {
    // varDef |
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
    KwToken("proc") ~> ident ^? (
      env.defineProcedure.andThen( procSymbol => {env.enterScope(); procSymbol} ), // define proc in outer scope, then enter proc-scope
      { name => s"$name is already defined" }
    )

  private def refParamDefHeader: Parser[RefParamSymbol] =
    KwToken("ref") ~> ident ^? (
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
    (KwToken("if") ~> boolExp <~ KwToken("then")) ~ rep(cmd) ~ (KwToken("else") ~> rep(cmd) <~ KwToken("fi")) ^^ {
      case e ~ cthen ~ cElse => If(e, cthen, cElse)
    } |
      (KwToken("if") ~> boolExp <~ KwToken("then")) ~ rep(cmd) <~ KwToken("fi") ^^ {
        case e ~ cthen => If(e, cthen, List())
      } |
      (KwToken("while") ~> boolExp <~ KwToken("do")) ~ rep(cmd) <~ KwToken("od") ^^ {
        case e ~ cmdList => While(e, cmdList)
      } |
      (KwToken("write") ~> LeftPToken("(") ~> arithExp) <~ RightPToken(")") <~ SemicolonToken(";") ^^ (e => Write(e)) |
      (KwToken("read") ~> LeftPToken("(") ~> locAccess) <~ RightPToken(")") <~ SemicolonToken(";") ^^ (e => Read(e)) |
      (lExp <~ AssignToken(":=")) ~ arithExp <~ SemicolonToken(";") ^^ {
        case ref ~ e =>
          Assign(ref, e)
      }| definedProc ~ (LeftPToken("(") ~> repsep(arithExp, CommaToken(",")) <~ RightPToken(")")) <~ SemicolonToken(";") ^^ {
        case ps ~ args =>

          // args.map(Arg(_, None))
          Call(ps, args.map(Arg(_, None)))
      }
  }
  // parse objects -----------------------------------------------------------------------------------------------------

/*
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

  def parse(str: String): Obj = {

    // remove trailing whitespaces before passing the input to the scanner
    val lexer: Reader[lexical.Token] = new lexical.Scanner(str.trim.stripSuffix(lexical.whitespacePattern))

    phrase(obj)(lexer) match { // phrase tries to analyse the whole text (the "phrase")
      case Success(tree,_)       => tree
      case error@NoSuccess(_,_)  =>
        println(error)
        throw new IllegalArgumentException("Parser Error")
    }

  }

}





























