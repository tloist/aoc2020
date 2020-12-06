object PasswordListParser {
  // Not that it has much worth here, just for trying it out
  opaque type Password = String
  object Password {
    // Interesting. Because outer classes are not aware, we need something to 'lift' values into the type
    // From the view of an insider this is basically just useless code…
    def apply(input: String): Password = input
  }

  // 1-3 a: abcde
  import cats._
  import cats.data._
  import cats.implicits._
  import cats.parse.{Parser, Parser1, Numbers, Rfc5234 => Rfc}

  case class ConstrainedChar(char: Char, min: Int, max: Int) {
    override def toString = s"Char['$char' ${min}x-${max}x]"

    def evaluate(pw: Password): Boolean =
      val charCount = pw.count(_ == char)
      min <= charCount && charCount <= max
  }
  case class InputLine(constrain: ConstrainedChar, password: Password) {
    override def toString = s"Input['$password' constrained by $constrain]"

    def isValid: Boolean = constrain.evaluate(password)
  }

  private[this] val whitespaces0: Parser[Unit] = Rfc.wsp.rep.void

  val constrainedCharParser: Parser1[ConstrainedChar] = {
    def numberParser = Numbers.digits1.map(_.toInt).surroundedBy(whitespaces0)
    def charParser = Rfc.char.surroundedBy(whitespaces0)
    (numberParser <* Parser.char('-'), numberParser, charParser).tupled.map((min, max, char) => ConstrainedChar(char, min, max))
  }

  val inputLineParser: Parser1[InputLine] = {
    val passwordParser: Parser1[String] = Rfc.alpha.rep1.string.surroundedBy(whitespaces0)
    (constrainedCharParser, Parser.char(':') *> passwordParser).tupled.map(InputLine.apply)
  }

  val inputLinesParser: Parser[List[InputLine]] = {
    def separator = Parser.oneOf(List(Rfc.crlf, Rfc.lf, Rfc.cr, Parser.char('\n')))
    val linesParser =  Parser.repSep(inputLineParser.surroundedBy(whitespaces0), 0, separator)
    linesParser <* separator.? 
  }

  def apply(input: String) = inputLinesParser.parse(input)
}