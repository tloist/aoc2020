object Part1 {
  def main(args: Array[String]): Unit = {
    val instructions = Input.asLines().map(Instruction.fromLine)
    val finalFerry = Ferry().execute(instructions)
    println(s"The ferry is at ${finalFerry.position} with distance ${finalFerry.position.distance()}")
  }
}