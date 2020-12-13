object Part1 {
  def main(args: Array[String]): Unit = {
    val counts = CustomsParser(Input.asString()).map(_.anyoneAnswered.size)
    println(s"Positive answered questions in sum ${counts.sum} = ${counts.mkString(" + ")}")
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val counts = CustomsParser(Input.asString()).map(_.everyoneAnswered.size)
    println(s"Positive answered questions in sum ${counts.sum} = ${counts.mkString(" + ")}")
  }
}
