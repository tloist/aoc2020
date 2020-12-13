import cats._
import cats.data._
import cats.implicits._

opaque type Question = Char

object Question {
  val validValues: Set[Question] = ('a' to 'z').toSet
  def apply(char: Char): Question = if validValues(char) then char else throw
    new IllegalArgumentException(s"Invalid character for question marker '$char'")
  given Ordering[Question] = Ordering.Char
}

case class Person(val acknowledgedQuestions: Set[Question]) {
  override def toString: String = s"Person[${acknowledgedQuestions.toList.sorted.mkString}]"
}
case class PersonGroup(val persons: NonEmptyList[Person]) {
  val unionMonoid: Monoid[Set[Question]] = MonoidK[Set].algebra
  val intersectionMonoid: Monoid[Set[Question]] = new Monoid[Set[Question]] {
    override def empty: Set[Question] = Question.validValues.toSet
    override def combine(x: Set[Question], y: Set[Question]): Set[Question] = x.intersect(y)
  }
  
  def anyoneAnswered: Set[Question] = combineQuestions(unionMonoid)
  def everyoneAnswered: Set[Question] = combineQuestions(intersectionMonoid)
  def size = persons.size
  
  private def combineQuestions(monoid: Monoid[Set[Question]]): Set[Question] =
    monoid.combineAll(persons.map(_.acknowledgedQuestions.toSet).toList)
}