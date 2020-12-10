
object Part1 {
  given PassportEvaluationPolicy with {
    import Passport.FieldName._
    def validate(passport: Passport): Boolean = List(passport(BirthYear), passport(IssueYear), passport(ExpirationYear),
      passport(Height), passport(HairColor), passport(EyeColor), passport(PassportId)).flatten.size == 7
  }
  
  def evaluteInput(body: List[Passport] => Unit): Unit =
    val input = Input.asString(TaskPart.First)
    PassportParser(input) match {
      case Right(passports) => body(passports)
      case Left(error) => println(s"Parsing errors: $error")
    }

  def main(args: Array[String]): Unit = evaluteInput { passports =>
    val count = passports.count(_.isValid)
    println(s"${passports.size} passports were read, where $count were valid!")
  }
  
  
}