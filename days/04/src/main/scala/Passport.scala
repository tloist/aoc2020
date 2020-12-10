import cats._
import cats.data._
import cats.implicits._

trait PassportEvaluationPolicy {
  def validate(passport: Passport): Boolean
}

object Passport {
  enum FieldName(val abbr: String) {
    case BirthYear extends FieldName("byr")
    case IssueYear extends FieldName("iyr")
    case ExpirationYear extends FieldName("eyr")
    case Height extends FieldName("hgt")
    case HairColor extends FieldName("hcl")
    case EyeColor extends FieldName("ecl")
    case PassportId extends FieldName("pid")
    case CountryId extends FieldName("cid")
  }
  case class Field(name: FieldName, raw: String)
  
  given Order[FieldName] = Order.by(_.abbr)  // No implicit order available, e.g. on 'ordinal'?
  given Order[Field] = Order.by(_.name)
  
  def fieldName(input: String): Option[FieldName] = FieldName.values.find(_.abbr == input)
}

case class Passport(val fields: cats.data.NonEmptyMap[Passport.FieldName, String]) {
  def isValid(using PassportEvaluationPolicy): Boolean =
    summon[PassportEvaluationPolicy].validate(this)
  
  // Maybe an 'export' could work here?
  def apply(name: Passport.FieldName): Option[String] = fields(name)

  override def toString: String = 
    s"Passport[${fields.toList.sorted.mkString(", ")}]"
}