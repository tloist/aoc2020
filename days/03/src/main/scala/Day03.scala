object Part1 {
  
  def main(args: Array[String]): Unit = { 
    val treesHit = Geology.parseFrom(Input.asString(TaskPart.First)).traceOrigin(Move(3,1)).count(_ == Field.Tree)
    println(s"Hit $treesHit trees on the way through this map")
  }
}