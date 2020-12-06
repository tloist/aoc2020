class PasswordListParserSpec extends munit.FunSuite {
  import cats.parse.{Parser, Parser1}
  import cats.data._
  import PasswordListParser._

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

  test("single constrain can be parsed") {
    checkParseFunc(constrainedCharParser, "1-3 a") { constrained =>
      assertEquals(constrained.char, 'a')
      assertEquals(constrained.min, 1)
      assertEquals(constrained.max, 3)
    }
  }

  test("single input line can be parsed") {
    checkParseFunc(inputLineParser, "1-3 a: ababa") { input =>
      val pw = PasswordListParser.Password("foo")
      assertEquals(input.constrain, ConstrainedChar('a', 1, 3))
      assertEquals(input.password, Password("ababa"))
    }
  }

  test("multiline input without newline can be parsed") {
    val multiline = """|1-3 a: abba
                       |2-2 c: acdc""".stripMargin
    checkParseFunc(inputLinesParser, multiline) { _.toList match
      case first :: second :: Nil =>
        assertEquals(first, InputLine(ConstrainedChar('a', 1, 3), Password("abba")))
        assertEquals(second, InputLine(ConstrainedChar('c', 2, 2), Password("acdc")))
      case strange => fail(s"Not a result list with 2 entries but ${strange.length} entries: $strange")
    }
  }

  test("multiline input will accept an empty line at the end") {
    val multiline = """|1-3 a: abba
                       |2-2 c: acdc
                       |""".stripMargin
    checkParseFunc(inputLinesParser, multiline) { _.toList match
      case first :: second :: Nil =>
        assertEquals(first, InputLine(ConstrainedChar('a', 1, 3), Password("abba")))
        assertEquals(second, InputLine(ConstrainedChar('c', 2, 2), Password("acdc")))
      case strange => fail(s"Not a result list with 2 entries but ${strange.length} entries: $strange")
    }
  }

}