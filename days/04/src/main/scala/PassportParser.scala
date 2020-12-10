import cats._
import cats.data._
import cats.implicits._
import cats.parse.{Parser, Parser1, Numbers, Rfc5234 => Rfc}
import cats.data.NonEmptyMap

object PassportParser {
  lazy val field: Parser1[NonEmptyMap[Passport.FieldName, String]] =
    (fieldValue <* Parser.char(':').void, fieldValue).tupled.map { (name, value) =>
      Passport.fieldName(name).map(f => NonEmptyMap.of((f, value))).getOrElse {
        throw new IllegalArgumentException(s"Can't parse ('$name':'$value') into a field-value pair!")
      }
    }
    
  lazy val fieldsLine: Parser1[NonEmptyMap[Passport.FieldName, String]] =
    Parser.rep1Sep(field, 1, whitespaces0).map(_.reduce)
  
  lazy val passport: Parser1[Passport] = {
    Parser.rep1Sep(fieldsLine, 1, newline).map(_.reduce).map(Passport.apply)
  }
  
  lazy val passports: Parser[List[Passport]] =
    Parser.repSep(passport, 0, newline.rep(2)) <* newline.rep.?

  def apply(input: String) = passports.parseAll(input)

  private[this] val fieldName: Parser1[String] = (Rfc.alpha, Rfc.alpha, Rfc.alpha).tupled.map((a,b,c) => s"$a$b$c").backtrack
  private[this] val alphaNum: Parser1[String] = Rfc.alpha.map(_.toString).orElse1(Numbers.digits1).orElse1(Parser.char('#').map(_.toString))
  private[this] val fieldValue: Parser1[String] = alphaNum.rep1.string
  private[this] val whitespaces0: Parser[Unit] = Rfc.wsp.rep.void
  private[this] val newline: Parser1[Unit] = Parser.char('\n').surroundedBy(whitespaces0).void
}
