object Part1 {
  def main(args: Array[String]): Unit = {
    val counts = CustomsParser(Input.asString()).map(_.anyOneAnswered.size)
    println(s"Positive answered questions in sum ${counts.sum} = ${counts.mkString(" + ")}")
  }
}
