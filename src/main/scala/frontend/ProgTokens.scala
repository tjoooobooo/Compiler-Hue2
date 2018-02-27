package frontend

import scala.util.parsing.combinator.token.Tokens

class ProgTokens extends Tokens {

  case class  NumberToken(chars: String)    extends Token
  case class  LeftPToken(chars: String)     extends Token
  case class  RightPToken(chars: String)    extends Token
  case class  AssignToken(chars: String)    extends Token
  case class  SemicolonToken(chars: String) extends Token
  case class  ColonToken(chars: String)     extends Token
  case class  CommaToken(chars: String)     extends Token
  case class  DotToken(chars: String)       extends Token
  case class  AddOpToken(chars: String)     extends Token
  case class  MultOpToken(chars: String)    extends Token
  case class  CompOpToken(chars: String)    extends Token
  case class  IdentToken(chars: String)     extends Token
  case class  StringToken(chars: String)    extends Token
  case class  KwToken(chars: String)        extends Token

}
