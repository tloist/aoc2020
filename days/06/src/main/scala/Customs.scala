import cats._
import cats.data._
import cats.implicits._

opaque type Question = Char

object Question {
  val validValues: Set[Char] = ('a' to 'z').toSet
  def apply(char: Char): Question = if validValues(char) then char else throw
    new IllegalArgumentException(s"Invalid character for question marker '$char'")
  given Ordering[Question] = Ordering.Char
}

case class Person(val acknowledgedQuestions: Set[Question]) {
  override def toString: String = s"Person[${acknowledgedQuestions.toList.sorted.mkString}]"
}
case class PersonGroup(val persons: NonEmptyList[Person]) {
  def anyOneAnswered: Set[Question] = persons.toList.toSet.flatMap(_.acknowledgedQuestions)
  def size = persons.size
}