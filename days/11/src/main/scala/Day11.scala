def determineLastState()(using Ferry.VisionMode): (Ferry, Int) = {
  val ferry = Ferry.fromString(Input.asString())
  val sanitized = ferry.states.take(5000)
  sanitized.zipWithIndex.last
}

object Part1 {
  given stratey as Ferry.VisionMode = Ferry.VisionMode.Direct
  
  def main(args: Array[String]): Unit = {
    val (last, index) = determineLastState()
    println(s"Step $index is stable and has ${last.countOccupiedSeats} seats")
  }
}

object Part2 {
  given stratey as Ferry.VisionMode = Ferry.VisionMode.Rays

  def main(args: Array[String]): Unit = {
    val (last, index) = determineLastState()
    println(s"Step $index is stable and has ${last.countOccupiedSeats} seats")
  }
}