import cats._
import cats.data._
import cats.implicits._
import cats.parse.{Parser, Parser1}
import Passport.FieldName._

class PassportParserSpec extends munit.FunSuite {

  def checkParseFunc[T](parser: Parser[T], input: String)(spec: T => Unit) = {
    parser.parseAll(input) match {
      case Right(result) => spec(result)
      case Left(Parser.Error(offset, expectations)) => 
        fail(s"""|Parsing of input '$input' failed at position: $offset!
                 |Input that failed: '${input.charAt(offset - 1)}'.
                 |Expected: $expectations}
                 |""".stripMargin
        )
    }
  }

  test("Single fieldname can be parsed") {
    checkParseFunc(PassportParser.field, "ecl:gry") { field =>
      assertEquals(field.size, 1L)
      assertEquals(field(EyeColor), Some("gry"))
    }
  }

  test("Multiple field values per line") {
    val line = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
    checkParseFunc(PassportParser.fieldsLine, line) { field =>
      assertEquals(field.size, 4L)
      assertEquals(field(EyeColor), Some("gry"))
      assertEquals(field(PassportId), Some("860033327"))
      assertEquals(field(ExpirationYear), Some("2020"))
      assertEquals(field(HairColor), Some("#fffffd"))
    }
  }
  
  test("Passport over multiple lines") {
    val line = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
                 |byr:1937 iyr:2017 cid:147 hgt:183cm
                 |
                 |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
                 |hcl:#cfa07d byr:1929
                 |
                 |hcl:#ae17e1 iyr:2013
                 |eyr:2024
                 |ecl:brn pid:760753108 byr:1931
                 |hgt:179cm
                 |
                 |hcl:#cfa07d eyr:2025 pid:166559648
                 |iyr:2011 ecl:brn hgt:59in""".stripMargin
    checkParseFunc(PassportParser.passports, line) { passports =>
      assertEquals(passports.size, 4)
      val last = passports.last
      assertEquals(last(HairColor), Some("#cfa07d"))
      assertEquals(last(ExpirationYear), Some("2025"))
      assertEquals(last(PassportId), Some("166559648"))
      assertEquals(last(IssueYear), Some("2011"))
      assertEquals(last(EyeColor), Some("brn"))
      assertEquals(last(Height), Some("59in"))
      assertEquals(last(CountryId), None)
    }
  }

  test("Passports over multiple lines with newline") {
    val line = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
                 |byr:1937 iyr:2017 cid:147 hgt:183cm
                 |
                 |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
                 |hcl:#cfa07d byr:1929
                 |
                 |hcl:#ae17e1 iyr:2013
                 |eyr:2024
                 |ecl:brn pid:760753108 byr:1931
                 |hgt:179cm
                 |
                 |hcl:#cfa07d eyr:2025 pid:166559648
                 |iyr:2011 ecl:brn hgt:59in
                 | 
                 |""".stripMargin
    checkParseFunc(PassportParser.passports, line) { passports =>
      assertEquals(passports.size, 4)
    }
  }
  
  test("All positive examples for part 2 are valid") {
    val line =
      """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
        |hcl:#623a2f
        |
        |eyr:2029 ecl:blu cid:129 byr:1989
        |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
        |
        |hcl:#888785
        |hgt:164cm byr:2001 iyr:2015 cid:88
        |pid:545766238 ecl:hzl
        |eyr:2022
        |
        |iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719""".stripMargin
    checkParseFunc(PassportParser.passports, line) { passports =>
      import Part2.given
      assertEquals(passports.size, 4)
      assertEquals(passports.count(_.isValid), 4)
    }
  }

  test("All negative examples for part 2 are invalid") {
    val line =
      """eyr:1972 cid:100
        |hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
        |
        |iyr:2019
        |hcl:#602927 eyr:1967 hgt:170cm
        |ecl:grn pid:012533040 byr:1946
        |
        |hcl:dab227 iyr:2012
        |ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
        |
        |hgt:59cm ecl:zzz
        |eyr:2038 hcl:74454a iyr:2023
        |pid:3556412378 byr:2007""".stripMargin
    checkParseFunc(PassportParser.passports, line) { passports =>
      import Part2.given
      assertEquals(passports.size, 4)
      assertEquals(passports.count(_.isValid), 0)
    }
  }
}