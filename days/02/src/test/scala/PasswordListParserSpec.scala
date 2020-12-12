import cats.parse.{Parser, Parser1}
import cats.data._
import PasswordListParser._
import ParserSpecs._

class PasswordListParserSpec extends munit.FunSuite {

  test("single constrain can be parsed") {
    checkParseFunc(constrainedCharParser, "1-3 a") { constrained =>
      assertEquals(constrained.char, 'a')
      assertEquals(constrained.c1, 1)
      assertEquals(constrained.c2, 3)
    }
  }

  test("single input line can be parsed") {
    checkParseFunc(inputLineParser, "1-3 a: ababa") { input =>
      assertEquals(input.constrain, ConstrainedChar('a', 1, 3))
      assertEquals(input.password, "ababa")
    }
  }

  test("multiline input without newline can be parsed") {
    val multiline = """|1-3 a: abba
                       |2-2 c: acdc""".stripMargin
    checkParseFunc(inputLinesParser, multiline) { _.toList match
      case first :: second :: Nil =>
        assertEquals(first, InputLine(ConstrainedChar('a', 1, 3), "abba"))
        assertEquals(second, InputLine(ConstrainedChar('c', 2, 2), "acdc"))
      case strange => fail(s"Not a result list with 2 entries but ${strange.length} entries: $strange")
    }
  }

  test("multiline input will accept an empty line at the end") {
    val multiline = """|1-3 a: abba
                       |2-2 c: acdc
                       |""".stripMargin
    checkParseFunc(inputLinesParser, multiline) { _.toList match
      case first :: second :: Nil =>
        assertEquals(first, InputLine(ConstrainedChar('a', 1, 3), "abba"))
        assertEquals(second, InputLine(ConstrainedChar('c', 2, 2), "acdc"))
      case strange => fail(s"Not a result list with 2 entries but ${strange.length} entries: $strange")
    }
  }

}