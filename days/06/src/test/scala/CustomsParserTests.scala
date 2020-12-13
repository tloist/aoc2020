import ParserSpecs._
import cats.data._

class CustomsParserTests extends munit.FunSuite {
  
  test("Parse single question") {
    checkParseFunc(CustomsParser.answer, "a") { question =>
      assertEquals(question, Question('a'))
    }
  }
  
  test("Parse single person") {
    checkParseFunc(CustomsParser.person, "abc") { result =>
      assertEquals(result, personWithQuestions("abc"))
    }
  }
  
  test("Parse a group of people") {
    val groupInput =
      """|abcx
         |abcy
         |abcz""".stripMargin
    checkParseFunc(CustomsParser.personGroup, groupInput) { group =>
      assertEquals(group, PersonGroup(NonEmptyList.fromListUnsafe(List(
        personWithQuestions("abcx"),
        personWithQuestions("abcy"),
        personWithQuestions("abcz")
      ))))
    }
  }
  
  test("Parse multiple groups with union count") {
    val multipleGroupsInput =
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin
    checkParseFunc(CustomsParser.personGroups, multipleGroupsInput) { groups =>
      assertEquals(groups.size, 5)
      assertEquals(groups.map(_.anyoneAnswered.size), List(3, 3, 3, 1, 1))
    }
  }

  test("Parse multiple groups with intersection count") {
    val multipleGroupsInput =
      """abc
        |
        |a
        |b
        |c
        |
        |ab
        |ac
        |
        |a
        |a
        |a
        |a
        |
        |b""".stripMargin
    checkParseFunc(CustomsParser.personGroups, multipleGroupsInput) { groups =>
      assertEquals(groups.size, 5)
      assertEquals(groups.map(_.everyoneAnswered.size), List(3, 0, 1, 1, 1))
    }
  }
  
  
  def personWithQuestions(questions: String): Person = Person(questions.map(Question.apply).toSet)
  
}
