import cats._
import cats.data._
import cats.implicits._
import PassportValidation._
import Passport.FieldName._

def evaluteInput(body: List[Passport] => Unit): Unit =
  val input = Input.asString(TaskPart.First)
  PassportParser(input) match {
    case Right(passports) => body(passports)
    case Left(error) => println(s"Parsing errors: $error")
  }

object Part1 {
  given PassportEvaluationPolicy with {
    def validate(passport: Passport): Boolean = List(passport(BirthYear), passport(IssueYear), passport(ExpirationYear),
      passport(Height), passport(HairColor), passport(EyeColor), passport(PassportId)).flatten.size == 7
  }

  def main(args: Array[String]): Unit = evaluteInput { passports =>
    println(s"${passports.size} passports were read, where ${passports.count(_.isValid)} were valid!")
  }
}

object Part2 {
  
  given PassportEvaluationPolicy with {
    def validate(passport: Passport): Boolean = validateBirthYear(passport)
        .flatMap(validateIssueYear).flatMap(validateExpirationYear).flatMap(validateHeight)
        .flatMap(validateHairColor).flatMap(validateEyeColor).flatMap(validatePassportId)
        .isRight
  }

  def main(args: Array[String]): Unit = evaluteInput { passports =>
    println(s"${passports.size} passports were read, where ${passports.count(_.isValid)} were valid!")
  }
}