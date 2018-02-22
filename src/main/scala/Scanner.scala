import AST._

/* WANDELT CODE IN TOKENS UM*/
object Scanner {
  private val numberS = """[1-9]\d+"""
  private val letterS = """[a-z|A-Z]+"""
  private val identS = """\w+"""
  private val expS = """+|-|*|/"""

  private val Number = numberS.r
  private val Letter = letterS.r
  private val Ident = identS.r
  private val Exp = expS.r

  private val tokenPatS = List(numberS,letterS,identS, expS).reduceRight(_ + "|" + _)
  private val tokenPat = tokenPatS.r

  def apply(code: String): List[(Int, Code)] = {
    var result: List[(Int, Code)] = List()
    var tokens = tokenPat.findAllIn(code)

    var counter = 1

    while (tokens.hasNext) {
      var token = nextToken
      result = result :+ (counter, token)
    }

    def nextToken: Code = {
      tokens.next() match {
        case Number(num) =>
          val exp1 = Number(num)
          val op = tokens.next()
          op match {
            case "+" => Add(exp1, Number(tokens.next()))
            case "*" => Mul(exp1, Number(tokens.next()))
          }
      }
    }
    result
  }
}
