import SpecPath.Step._
import SeatSpecificationParser._
import ParserSpecs._
import cats.data._

class SeatSpecificationParserTests extends munit.FunSuite {
  
  test("Parse example input") {
    checkParseFunc(lineParser, "FBFBBFFRLR") { result =>
      assertEquals(result, NonEmptyList.of(Front, Back, Front, Back, Back, Front, Front, Right, Left, Right))
    }
  }
  
  test("Parse multiple lines") {
    val input =
      """|FBFBBFFRLR
         |BFFFBBFRRR
         |BBFFBBFRLL
         |FFFBBBFRRR""".stripMargin
    checkParseFunc(linesParser, input) { result =>
      println(result)
    }
  }
  
  
  
}
