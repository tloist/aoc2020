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
       
    /* Relevant documentation:
     * * https://typelevel.org/cats/typeclasses/monoidk.html
     * * https://typelevel.org/cats/datatypes/kleisli.html
     * 
     * Below is a table of some of the type class instances Kleisli can have depending on what instances F[_] has.
     *
     *   Type class     | Constraint on `F[_]`
     *   -------------- | -------------------
     *         ... other type classes ...
     *   MonoidK*       | Monad
     * 
     * *These instances only exist for Kleisli arrows with identical input and output types;
     *  that is, Kleisli[F, A, A] for some type A. These instances use Kleisli composition as the combine operation,
     *  and Monad.pure as the empty value.
     */
    
    type ValidationResultKleisli[A] = Kleisli[ValidationResult, A, A]
    val theMonad = summon[Monad[ValidationResult]]
    
    /* If aboves hold true, we have a Kleisli[F, A, A] with F is ValidationResult
     * Also we just summoned a Monad for ValidationResult
     * => this should mean, that there is a MonadK for it, isn't it?
     */
    
    // Compiles says 'no' to this one?!
    // val _ = summon[MonoidK[ValidationResultKleisli]]
    
    // And if there is, there should be a 'combineAll' method for VAlidationResultKleisli[A] for all A
    
    def validate(passport: Passport): Boolean = {
      val funcs: List[ValidationResultKleisli[Passport]] = List(validateBirthYear, validateIssueYear, validateExpirationYear, validateHeight,
        validateHairColor, validateEyeColor, validatePassportId).map(Kleisli.apply)
      // to reduce them, it would help to have a combineAll function available. But because of above: 'no'
      //val combined = combineAll(funcs)
      
      val combined: ValidationResultKleisli[Passport] = funcs.reduce(_.andThen(_))
      combined.run(passport).isRight
    }
  }

  def main(args: Array[String]): Unit = evaluteInput { passports =>
    println(s"${passports.size} passports were read, where ${passports.count(_.isValid)} were valid!")
  }
}