class ProgLexical extends ProgTokens {

  private val numberPatS  = """(0|(?:[1-9][0-9]*))"""
  private val stringPatS  = """\"([^"]*)\""""
  private val idPatS      = """(\w+)"""
  private val addOpPatS   = """(\+|\-)"""
  private val multOpPatS  = """(\*|/)"""
  private val compOpPatS  = """(<=|>=|=|<|>)"""
  private val assignPatS  = """(:=)"""
  private val keywordPatS = """(OBJECT|BEGIN|INIT|END|VAR|IF|THEN|ELSE|FI|WHILE|DO|OD|WRITE|TYPE|INT|PROC|REF|IMPORT|EXPORT)"""

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
  private val SemicolonPat = semicolonPatS.r
  private val ColonPat     = colonPatS.r
  private val CommaPat     = commaPatS.r
  private val DotPat       = dotPatS.r

  private val pats = List( KeywordPat, NumberPat, AddOpPat, MultOpPat, LeftPPat, RightPPat, AssignPat, SemicolonPat, ColonPat, CommaPat, DotPat, CompOpPat, IdPat )

  private val whitespacePatS= """\s+"""

  private val whitespacePat = whitespacePatS.r

}
