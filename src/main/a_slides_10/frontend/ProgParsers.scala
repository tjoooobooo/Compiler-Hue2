package slides_10.frontend

import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.input.Reader

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

  def arithExp: Parser[ArithExp] = chainl1(term, term, addOp)

  def addOp : Parser[(ArithExp, ArithExp) ⇒ ArithExp] =
    AddOpToken("+") ^^^ {(x:ArithExp, y: ArithExp) => Add(x,y)} |
    AddOpToken("-") ^^^ {(x:ArithExp, y: ArithExp) => Sub(x,y)}

  def term = chainl1(factor, factor, multOp)

  def multOp : Parser[(ArithExp, ArithExp) ⇒ ArithExp] =
    MultOpToken("*") ^^^ {(x:ArithExp, y: ArithExp) => Mul(x,y)}  |
    MultOpToken("/") ^^^ {(x:ArithExp, y: ArithExp) => Div(x,y)}

  def factor: Parser[ArithExp] =
    number   |
    LeftPToken("(") ~> arithExp <~ RightPToken(")") |
    refExp

  def refExp: Parser[Ref] =
    lExp ^^ {le => Ref(le) }

  def lExp: Parser[RefExp] =
    ident ^?  ({  // check whether ident is a defined variable
      case name if (
       env.lookup(name) match {
        case scala.util.Success(Variable(_)) => true
        case scala.util.Failure(_) => false
      })
       => VarRef(env.lookup(name).get.asInstanceOf[Variable]) // ident is a defined variable create AST-node using its definition
    },
      name => s"undefined name '$name'" // ident is not a defined variable
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
      env.leaveScope
      (defs, cmds)
    }


  // parse definitions -------------------------------------------------------------------------------------------------
  def definition: Parser[Definition] =
    varDefHeader ~ (AssignToken(":=") ~> arithExp <~ SemicolonToken(";")) ^^ {
      case vari ~ e => VarDef(vari, e)
    }

  def varDefHeader: Parser[Variable] =
    (KwToken("VAR") ~> ident) ^? (
      defineVariable,  // partial function that fails if the name is already defined in the current environment
      { case name => s"$name is alredy defined" }
    )


  // helper for variable definitions: Adapt "Try logic" of static environment to the "PartialFunction logic" of parser combinators,
  // Partial function that is undefined on names that can not be defined in the current env.
  // Attention, stateful object: isDefinedAt MUST be called immediately before calling apply.
  object defineVariable extends PartialFunction[String, Variable] {
    var symbol: Variable = _
    override def isDefinedAt(x: String): Boolean = x match {
      case name =>
        symbol = Variable(name)
        env.define(name, symbol) match {
          case scala.util.Success(_) =>  true
          case scala.util.Failure(_) => false
        }
    }
    override def apply(x: String): Variable = x match {
      case name => symbol
    }
  }

  // parse commands ----------------------------------------------------------------------------------------------------
  def cmd: Parser[Cmd] =
    (KwToken("IF")~>boolExp<~KwToken("THEN")) ~ rep(cmd) ~ (KwToken("ELSE")~>rep(cmd)<~KwToken("FI")) ^^ { case e~cthen~cElse => If(e, cthen, cElse) } |
    (KwToken("IF")~>boolExp<~KwToken("THEN")) ~ rep(cmd) <~KwToken("FI")     ^^ { case e~cthen => If(e, cthen, List()) } |
    (KwToken("WHILE")~>boolExp<~KwToken("DO")) ~ rep(cmd) <~ KwToken("OD")   ^^ { case e ~ cmdList => While(e, cmdList) } |
    (KwToken("WRITE")~>LeftPToken("(")~> arithExp) <~ RightPToken(")") <~ SemicolonToken(";") ^^ { case e => Write(e) } |
    (lExp <~ AssignToken(":=")) ~ arithExp <~ SemicolonToken(";")            ^^ { case ref~e => Assign(ref, e) }


  def parse(str: String)  = {

    // remove trailing whitespaces before passing the input to the scanner
    val lexer: Reader[lexical.Token] = new lexical.Scanner(str.trim.stripSuffix(lexical.whitespacePattern))

    phrase(prog)(lexer) match { // phrase tries to analyse the whole text (the "phrase")
      case Success(tree,_)       => tree
      case error@NoSuccess(_,_)  => println(error)
        throw new IllegalArgumentException("Parser Error")
    }
  }

}
