import cats._
import cats.data._
import cats.implicits._
import PassportValidation._
import Passport.FieldName._

class PassportValidationSpec extends munit.FunSuite {

  test("Validation Birth Year positive") {
    assertValid(validateBirthYear)(BirthYear, "2002")
  }

  test("Validation Birth Year negative") {
    assertInvalid(validateBirthYear)(BirthYear, "2003")
  }

  test("Validation Height positive #1") {
    assertValid(validateHeight)(Height, "60in")
  }

  test("Validation Height positive #2") {
    assertValid(validateHeight)(Height, "190cm")
  }

  test("Validation Height negative #1") {
    assertInvalid(validateHeight)(Height, "190in")
  }

  test("Validation Height negative #2") {
    assertInvalid(validateHeight)(Height, "190")
  }

  test("Validation Hair Color positive") {
    assertValid(validateHairColor)(HairColor, "#123abc")
  }

  test("Validation Hair Color negative #1") {
    assertInvalid(validateHairColor)(HairColor, "#123abz")
  }

  test("Validation Hair Color negative #2") {
    assertInvalid(validateHairColor)(HairColor, "123abc")
  }

  test("Validation Eye Color positive") {
    assertValid(validateEyeColor)(EyeColor, "brn")
  }

  test("Validation Eye Color negative") {
    assertInvalid(validateEyeColor)(EyeColor, "wat")
  }

  test("Validation Passport ID positive") {
    assertValid(validatePassportId)(PassportId, "000000001")
  }

  test("Validation Passport ID negative") {
    assertInvalid(validatePassportId)(PassportId, "0123456789")
  }
  
  def assertValid(function: Passport => Either[PassportValidation, Passport]) = validate(true, function)
  def assertInvalid(function: Passport => Either[PassportValidation, Passport]) = validate(false, function)
  
  def validate(valid: Boolean, validation: Passport => Either[PassportValidation, Passport])(field: Passport.FieldName, value: String): Unit =
    validation(passportWith(field, value)) match {
      case Left(error) => s"Validation error due to $error"
      case Right(passport) => ()
    }
  
  def passportWith(field: Passport.FieldName, value: String): Passport =
    Passport(NonEmptyMap.one(field, value))
}
