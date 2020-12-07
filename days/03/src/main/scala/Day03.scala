
val geo = Geology.parseFrom(Input.asString(TaskPart.First))
def calculateTreesHitBy(move: Move) = geo.traceOrigin(move).count(_ == Field.Tree)

object Part1 {
  def main(args: Array[String]): Unit = { 
    val treesHit = calculateTreesHitBy(Move(3,1))
    println(s"Hit $treesHit trees on the way through this map")
  }
}
object Part2 {
  def main(args: Array[String]): Unit = {
    val product = List(Move(1,1), Move(3,1), Move(5,1), Move(7,1), Move(1,2)).map { move =>
      val trees = calculateTreesHitBy(move)
      println(s"Trees hit by $move are: $trees")
      trees
    }.foldLeft(1)(_ * _)
    println(s"Total product is: $product")
  }
  
}