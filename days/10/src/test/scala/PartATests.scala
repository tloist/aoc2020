import better.files.Resource

class PartATests extends munit.FunSuite {
  val example1 = Resource.getAsString("example1.txt").linesIterator.map(_.toInt).toList
  val example2 = Resource.getAsString("example2.txt").linesIterator.map(_.toInt).toList

  test("Example 1 works") {
    val result = calculateDifferences(example1)
    assertEquals(result.ones, 7)
    assertEquals(result.threes, 5)
    assertEquals(result.max, 19)
  }

  test("Example 2 works") {
    val result = calculateDifferences(example2)
    assertEquals(result.ones, 22)
    assertEquals(result.threes, 10)
    assertEquals(result.max, 49)
  }
}
