package frontend

import scala.util.matching.Regex
import scala.util.parsing.input.{Position, Reader}


/*
 * Lexical analysis
 */
class ProgLexical extends ProgTokens {

  private val numberPatS  = """(0|-?(?:[1-9][0-9]*))"""
  private val idPatS      = """(\w+)"""
  private val addOpPatS   = """(\+|\-)"""
  private val multOpPatS  = """(\*|/|%)"""
  private val compOpPatS  = """(<=|>=|==|<|>|!=)"""
  private val bitwiseOpPatS = """(&|\||\^|<<|>>)""" // neu
  private val assignPatS  = """(:=)"""
  private val keywordPatS = """(proc|int|object|begin|end|var|if|fi|then|else|while|do|od|write|read|ref|init)"""
  private val commentPatS = """(\/\/.*)"""
  private val commenBigPatS = """\/\*([\s\S]*?)\*\/"""

  private val leftPPatS      = """(\()"""
  private val rightPPatS     = """(\))"""
  private val semicolonPatS  = """(;)"""
  private val colonPatS      = """(:)"""
  private val commaPatS      = """(,)"""
  private val dotPatS        = """(\.)"""


  private val NumberPat    = numberPatS.r
  private val IdPat        = idPatS.r
  private val KeywordPat   = keywordPatS.r
  private val AddOpPat     = addOpPatS.r
  private val MultOpPat    = multOpPatS.r
  private val LeftPPat     = leftPPatS.r
  private val RightPPat    = rightPPatS.r
  private val AssignPat    = assignPatS.r
  private val CompOpPat    = compOpPatS.r
  private val BitwiseOpPat = bitwiseOpPatS.r
  private val SemicolonPat = semicolonPatS.r
  private val ColonPat     = colonPatS.r
  private val CommaPat     = commaPatS.r
  private val DotPat       = dotPatS.r

  private val pats = List(KeywordPat, NumberPat, AddOpPat, MultOpPat, LeftPPat, RightPPat, AssignPat,
    SemicolonPat, ColonPat, CommaPat, DotPat, BitwiseOpPat, CompOpPat, IdPat )

  private val whitespacePatS= """\s+"""

  private val whitespacePat = whitespacePatS.r

  def whitespacePattern: String = whitespacePatS

  private class ExpPosition(s: String, offset: Int) extends Position {
    var lineNr = 0
    var colNr  = 0
    var pos = 0
    var lineStart = 0
    while (pos < offset) {
      if (s.charAt(pos) == '\n') {
        lineStart = pos
        lineNr = lineNr + 1
        colNr = 0
      } else {
        colNr = colNr + 1
      }
      pos = pos + 1
    }
    override def line: Int = lineNr
    override def column: Int = colNr
    override def lineContents: String = s.substring(lineStart, pos)
  }

  /**
    * A "functional" scanner defined as Token reader.
    * Defined according to the example of scala.util.parsing.combinator.lexical.Scanners.Scanner
    * @param input    the string that is to be tokenized. Always passed unmodified to other scanners.
    * @param actPos   the start position within input, stat scanning at this position
    */
  class Scanner(input: String, private val actPos: Int = 0) extends Reader[Token] {
   var input2: String = input.replaceAll(commentPatS,"").replaceAll(commenBigPatS,"")

    // the position at which we look for a token
    private var actOffset = actPos

    // moves actOffset over whitespaces
    private def skipWhiteSpace(): Int =
      whitespacePat.findPrefixMatchOf(input2.subSequence(actOffset, input2.length())) match {
        case Some(m) => actOffset + m.end
        case None => actOffset
      }

    // Try to match r at actOffset with the input
    private def matchRegex(r: Regex): Option[String] = {
      r.findPrefixMatchOf(input2.subSequence(actOffset, input2.length())) match {
        case Some(matchedMatch) =>
          val res = Some(input2.subSequence(actOffset, actOffset + matchedMatch.end).toString)
          actOffset = actOffset + matchedMatch.end
          res
        case None => None
      }
    }

    // skip whitespaces at the front
    actOffset = skipWhiteSpace()

    // look for a token
    private var matched: Option[String] = None
    private val startOffset = actOffset // fix actual position
    private var tokenIndex = 0
    // !matched.isDefined
    while (matched.isEmpty && tokenIndex < pats.length) {
      matched = matchRegex(pats(tokenIndex))
      if (matched.isEmpty) { actOffset = startOffset }
      tokenIndex = tokenIndex+1
    }

    // fix token that was found and its position (if some token was found), and the position at its end
    val (tok: Token, tok_end: Int, tokPos: Position) = matched match {
      case None =>
        if (actOffset >= input2.length)
          (EOF, input2.length, new ExpPosition(input2, actOffset))
        else (errorToken("unexpected end of input"), input2.length)
      case Some(matchedStr) =>
        val pos = new ExpPosition(input2, actOffset)
        matchedStr match {
          case NumberPat(num)   => (NumberToken(num), actOffset, pos)
          case AddOpPat(op)     => (AddOpToken(op), actOffset, pos)
          case MultOpPat(op)    => (MultOpToken(op), actOffset, pos)
          case LeftPPat(p)      => (LeftPToken(p), actOffset, pos)
          case RightPPat(p)     => (RightPToken(p), actOffset, pos)
          case KeywordPat(p)    => (KwToken(p), actOffset, pos)
          case IdPat(p)         => (IdentToken(p), actOffset, pos)
          case SemicolonPat(p)  => (SemicolonToken(p), actOffset, pos)
          case ColonPat(p)      => (ColonToken(p), actOffset, pos)
          case CommaPat(p)      => (CommaToken(p), actOffset, pos)
          case DotPat(p)        => (DotToken(p), actOffset, pos)
          case CompOpPat(p)     => (CompOpToken(p), actOffset, pos)
          case BitwiseOpPat(p)  => (BitwiseOpToken(p), actOffset, pos)
          case AssignPat(p)     => (AssignToken(p), actOffset, pos)
          case x                => (ErrorToken("unexpected "+x), actOffset, pos)
        }
    }

    //watch for trailing whitespaces in the source
    //actOffset = skipWhiteSpace()


    // methods defined by reader
    override def pos: Position = tokPos

    override def first: Token = tok

    override def rest: Reader[Token] = new Scanner(input2, actOffset)

    override def atEnd: Boolean = {
      actPos >= input2.length
    }
  }

}
