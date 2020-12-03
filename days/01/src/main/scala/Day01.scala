extension (input: List[Int]) {
  def product: Int = input.foldLeft(1)(_ * _)

  def matchingTwoCombinations: Iterator[List[Int]] = input
    .combinations(2)
    .filter(_.sum == 2020)

  def matchingThreeCombinations: Iterator[List[Int]] = input
    .combinations(3)
    .filter(_.sum == 2020)
}

extension (solutions: Iterator[List[Int]]) {
  def print: Unit = solutions.foreach(numbers =>
    println(s"\tSolution ${numbers.mkString(" + ")} = 2020\n\twill multiply into: ${numbers.product}")
  )
}

def numbers = Input.asLines(TaskPart.First).map(_.toInt)

object Part1 {

  def main(args: Array[String]): Unit =
    numbers.matchingTwoCombinations.print
}

object Part2 {

  def main(args: Array[String]): Unit = 
    numbers.matchingThreeCombinations.print

}