import better.files.Resource
import xmascrypt._

class Example1Tests extends munit.FunSuite {
  val example1 = Resource.getAsString("example1.txt").split("\n").map(_.toInt).toList
  
  test("Example input finds out 127") {
    val results = xmasVerify(5)(example1)
      .filter(!_.isValid)
    
    assertEquals(results.size, 1)
    assertEquals(results.head.number, 127)
    assertEquals(results.head.previous, List(95, 102, 117, 150, 182))
  }
}

class Example2Tests extends munit.FunSuite {
  val example1 = Resource.getAsString("example1.txt").split("\n").map(_.toInt).toList

  test("Example input finds a contiguous set") {
    val result = findContiguousSetSumUpTo(127, example1)
    assertEquals(result.nonEmpty, true)
    val Some(min, max) = result
    assertEquals(min, 15)
    assertEquals(max, 47)
  }
}