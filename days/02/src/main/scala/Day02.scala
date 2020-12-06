import cats.parse._

object Part1 {

  def evaluate(input: String): Either[Parser.Error, Int] = PasswordListParser(input).map(_._2.count(_.isValid))

  def main(args: Array[String]): Unit = {
    val input = Input.asString(TaskPart.First)
    evaluate(input) match {
      case Right(count) => println(s"$count passwords were valid out of ${input.split("\n").size}!")
      case Left(error) => println(s"Parsing errors: $error")
    }
  }
}