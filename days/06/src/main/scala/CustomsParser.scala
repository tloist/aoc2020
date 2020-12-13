import cats._
import cats.data._
import cats.implicits._
import cats.parse.{Numbers, Parser, Parser1, Rfc5234 => Rfc}

object CustomsParser {

  private[this] val newline: Parser1[Unit] = Parser.char('\n')
  val answer: Parser1[Question] = Parser.charIn(Question.validValues).map(c => Question(c))

  val person: Parser1[Person] = Parser.rep1(answer, 1).map(qs => Person(qs.toList.toSet)) <* newline.?
  val personGroup: Parser1[PersonGroup] = person.rep1.map(ps => PersonGroup(ps))
  val personGroups: Parser[List[PersonGroup]] = Parser.repSep(personGroup, 1, newline)

  def apply(input: String): List[PersonGroup] = personGroups.parseAll(input).left.map { error =>
    new IllegalArgumentException(s"Could not parse due to $error")
  }.toTry.get
}
