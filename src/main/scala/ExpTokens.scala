import scala.util.parsing.combinator.token.Tokens

class ExpTokens extends Tokens {
  case class NumberToken(chars: String) extends Token
  case class LeftPToken(chars: String) extends Token
  case class RightPToken(chars: String) extends Token
  case class AddOpToken(chars: String) extends Token
  case class MultOpToken(chars: String) extends Token
}
