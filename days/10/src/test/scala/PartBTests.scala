import better.files.Resource

class PartBTests extends munit.FunSuite {
  val example1 = Resource.getAsString("example1.txt").linesIterator.map(_.toInt).toList
  val example2 = Resource.getAsString("example2.txt").linesIterator.map(_.toInt).toList
  
  test("Variants for example 1") {
    val variants = calculateAllVariantsNaively(example1)
    assertEquals(variants.size, 8)
  }
  
  test("Variants for example 2") {
    val variants = calculateAllVariantsNaively(example2)
    assertEquals(variants.size, 19208)
  }
  
  test("Variants for example 1 with cluster split") {
    val clusters = calculateNumberOfVariants(example1)
    val total = clusters.values.foldLeft(1)(_ * _)
    assertEquals(clusters.size, 2)
    assertEquals(total, 8)
  }

  test("Variants for example 2 with cluster split") {
    val clusters = calculateNumberOfVariants(example2)
    val total = clusters.values.foldLeft(1)(_ * _)
    assertEquals(clusters.size, 6)
    assertEquals(total, 19208)
  }
}