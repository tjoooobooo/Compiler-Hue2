object ExpScanner {
  // Definition of Tokens
  sealed abstract class Token
  case class NumberToken(d: Int) extends Token
  case class OperatorToken(c: Char) extends Token
  case object LeftPToken extends Token
  case object RightPToken extends Token
  case object EOFToken extends Token
}
