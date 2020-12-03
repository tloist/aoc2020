extension (input: List[Int]) {
  def product: Int = input.foldLeft(1)(_ * _)
}

def numbers = Input.asLines(TaskPart.First).map(_.toInt)

object Part1 {

  def main(args: Array[String]): Unit = numbers
    .combinations(2)
    .filter(_.sum == 2020)
    .foreach(numbers =>
      println(s"\tSolution ${numbers.mkString(" + ")} = 2020\n\twill multiply into: ${numbers.product}")
    )
}

object Part2 {

  def main(args: Array[String]): Unit = numbers
    .combinations(3)
    .filter(_.sum == 2020)
    .foreach(numbers =>
      println(s"\tSolution ${numbers.mkString(" + ")} = 2020\n\twill multiply into: ${numbers.product}")
    )

}