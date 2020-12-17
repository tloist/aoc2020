case class Summary(ones: Int, threes: Int, max: Int)

def calculateDifferences(list:List[Int]): Summary = {
  val completeList = (0 :: list).sorted
  val diffs = (0 :: list).sorted.sliding(2,1).map(l => l(1) - l(0)).toList
  Summary(diffs.count(_ == 1), diffs.count(_ == 3) + 1, completeList.last)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val numbers = Input.asLines().map(_.toInt)
    val summary = calculateDifferences(numbers)
    println(s"Differences -- Ones: ${summary.ones} / Threes: ${summary.threes} => ${summary.ones * summary.threes}")
  }
}