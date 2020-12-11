import cats._
import cats.data._
import cats.implicits._
import Passport._
import Passport.FieldName._

object PassportValidation {
  
  enum PassportValidation {
    case MissingRequiredValue(val field: FieldName)
    case NonNumericalValue(val field: FieldName, text: String)
    case ValueOutOfRange(val field: FieldName, value: Int, min: Int, max: Int)
    case ValueOutOfRangeInUnit(val field: FieldName, unit: String, value: Int, min: Int, max: Int)
    case NonHexColor(val field: FieldName, text: String)
    case ValueUnknown[T](val field: FieldName, value: T, known: Set[T])
    
    def field: FieldName
    def errorMessage: String = this match {
      case MissingRequiredValue(field) => s"Missing required field '$field' within passport"
      case NonNumericalValue(field, text) => s"Expected a numerical value within field $field but got '$text'!"
      case ValueOutOfRange(field, value, min, max) => s"Value $value within $field is without bounds! Should be: $min ≤ $value ≤ $max"
      case ValueOutOfRangeInUnit(field, unit, value, min, max) => s"Value $value [$unit] within $field is without bounds! Should be: $min ≤ $value ≤ $max"
      case NonHexColor(field, text) => s"Value $text in field $field has a non hex color value!"
      case ValueUnknown(field, value, known) => s"Value $value in field $field is unknown! Known ones are " + known.map(_.toString).toList.sorted.mkString(", ")
    }
  }
  
  import PassportValidation._
  type ValidationResult[A] = Either[PassportValidation, A]
  
  def validateBirthYear(passport: Passport): ValidationResult[Passport] = 
    validateNumberInBetween(passport, BirthYear)(1920, 2002)
  
  def validateIssueYear(passport: Passport): ValidationResult[Passport] =
    validateNumberInBetween(passport, IssueYear)(2010, 2020)
    
  def validateExpirationYear(passport: Passport): ValidationResult[Passport] =
    validateNumberInBetween(passport, ExpirationYear)(2020, 2030)
    
  def validateHeight(passport: Passport): ValidationResult[Passport] = passport(Height) match
    case None => Either.left(MissingRequiredValue(BirthYear))
    case Some(text) =>
      val nr = text.takeWhile(_.isDigit).toInt
      val unit = text.stripPrefix(text.takeWhile(_.isDigit))
      if knownLengthUnits(unit) then 
        val (min, max) = unit match
          case "cm" => (150, 193)
          case "in" => (59, 76)
        if (min <= nr && nr <= max) Either.right(passport)
        else Either.left(ValueOutOfRangeInUnit(Height, unit, nr, min, max))
      else Either.left(ValueUnknown(Height, unit, knownLengthUnits))
      
  def validateHairColor(passport: Passport): ValidationResult[Passport] = validateText(passport, HairColor) { text =>
    if text.startsWith("#") && text.tail.forall(c => ('a' to 'f').contains(c) || c.isDigit) then Either.right(passport)
    else Either.left(NonHexColor(HairColor, text))
  }
  
  def validatePassportId(passport: Passport): ValidationResult[Passport] = validateText(passport, PassportId) { text =>
    if text.length == 9 && text.forall(_.isDigit) then Either.right(passport)
    else Either.left(NonNumericalValue(PassportId, text))
  }
  
  def validateEyeColor(passport: Passport): ValidationResult[Passport] = validateText(passport, EyeColor) {
    case known if knownEyeColors(known) => Either.right(passport)
    case unknown => Either.left(ValueUnknown(EyeColor, unknown, knownEyeColors))
  }

  private[this] def validateNumberInBetween(passport: Passport, field: FieldName)(min: Int, max: Int): ValidationResult[Passport] =
    validateInt(passport, field) { no =>
      if (min <= no && no <= max) Either.right(passport)
      else Either.left(ValueOutOfRange(field, no, min, max))
    }

  private[this] def validateInt(passport: Passport, field: FieldName)(body: Int => ValidationResult[Passport]): ValidationResult[Passport] =
    validateText(passport, field) {
      case digits if digits.forall(_.isDigit) => body(digits.toInt)
      case text => Either.left(NonNumericalValue(field, text))
    }

  private[this] def validateText(passport: Passport, field: FieldName)(body: String => ValidationResult[Passport]): ValidationResult[Passport] =
    passport(field).map(body).getOrElse(Either.left(MissingRequiredValue(field)))

  private[this] val knownEyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  private[this] val knownLengthUnits = Set("cm", "in")
}
