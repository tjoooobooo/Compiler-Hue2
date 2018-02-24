import scala.util.parsing.input.{Position, Reader}
class ExpLexical extends ExpTokens {

  class Scanner(input: String) extends Reader[Token] {
    override def pos: Position = ???

    override def first: Token = ???

    override def rest: Reader[Token] = ???

    override def atEnd: Boolean = ???
  }

}
