import cats.parse._
import PasswordListParser._

def evaluate(input: String)(using PasswordPolicy): Either[Parser.Error, Int] = PasswordListParser(input).map(_._2.count(_.isValid))

def evaluteInput(using PasswordPolicy): Unit =
  val input = Input.asString(TaskPart.First)
  evaluate(input) match {
    case Right(count) => println(s"$count passwords were valid out of ${input.split("\n").size}!")
    case Left(error) => println(s"Parsing errors: $error")
  }

object Part1 {

  given PasswordPolicy with {
    def validate(password: String, constrain: ConstrainedChar): Boolean =
      val count: Int = password.count(_ == constrain.char)
      constrain.c1 <= count && count <= constrain.c2
  }

  def main(args: Array[String]): Unit = evaluteInput
}

object Part2 {
  given PasswordPolicy with {
    def validate(password: String, constrain: ConstrainedChar): Boolean =
      val pos1 = password.toList.lift(constrain.c1-1).map(_ == constrain.char).getOrElse(false)
      val pos2 = password.toList.lift(constrain.c2-1).map(_ == constrain.char).getOrElse(false)
      pos1 ^ pos2
  }

  def main(args: Array[String]): Unit = evaluteInput
}