import better.files.Resource
import Direction._

class FerryTests extends munit.FunSuite {
  val example1 = Resource.getAsString("example1.txt").linesIterator.map(Instruction.fromLine).toList
  
  test("Example #1") {
    val result = Ferry().execute(example1)
    assertEquals(result.position, Position(17, 8))
    assertEquals(result.position.distance(), 25)
    assertEquals(result.heading, South)
  }

}
