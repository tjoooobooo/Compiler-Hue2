import AST._

/* WANDELT CODE IN TOKENS UM*/
object Scanner {
  private val numberS = """(0|(?:[1-9][0-9]*))"""
  private val letterS = """[a-z|A-Z]+"""
  private val identS = """\w+"""
  private val operatorS = """(\+|\-|\*|/)"""

  private val Number = numberS.r
  private val Letter = letterS.r
  private val Ident = identS.r
  private val Operator = operatorS.r

  private val tokenPatS = List(numberS,letterS,identS, operatorS).reduceRight(_ + "|" + _)
  private val tokenPat = tokenPatS.r

  import ExpScanner._

  def apply(code: String): List[(Int, Token)] = {
    var result: List[(Int, Token)] = List()
    var tokens = tokenPat.findAllIn(code)

    var counter = 1

    while (tokens.hasNext) {
      var token = nextToken
      result = result :+ (counter, token)
    }

    def nextToken: Token = {
      if(tokens.hasNext)
      tokens.next() match {
        case Number(num) => NumberToken(num.toInt)
        case Operator(op) => OperatorToken(op.charAt(0))
        case x => throw new Exception("??: "+x)
      }
      else EOFToken
    }
    result
  }
}
