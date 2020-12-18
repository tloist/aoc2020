case class Summary(ones: Int, threes: Int, max: Int)

opaque type Cluster = List[Int]
extension (cluster: Cluster) {
  def added(no: Int): Cluster = cluster :+ no
  def max: Int = cluster.max
  def size: Int = cluster.size
  def head: Int = cluster.head
  def tail: List[Int] = cluster.tail

  def toString: String = s"Cluster(${cluster.mkString(", ")})"
}

def calculateDifferences(list: List[Int]): Summary = {
  val completeList = (0 :: list).sorted
  val diffs = (0 :: list).sorted.sliding(2,1).map(l => l(1) - l(0)).toList
  Summary(diffs.count(_ == 1), diffs.count(_ == 3) + 1, completeList.last)
}

private def variantRec(remaining: List[Int], current: List[Int]): List[List[Int]] = {
  if remaining.isEmpty then List(current)
  else remaining.takeWhile(_ - current.lastOption.getOrElse(0) <= 3).flatMap { next =>
    variantRec(remaining.dropWhile(_ <= next), current :+ next)
  }
}

/**
 * Calculates all variants by listing them up.
 * Naively means that it consumes too much heap and combinatorial space.
 */
def calculateAllVariantsNaively(list: List[Int]): List[List[Int]] = variantRec(list.sorted, List.empty)

def calculateNumberOfVariants(input: List[Int]): Map[Cluster, Int] = {
  val finalElement = input.max + 3
  val list = 0 :: input.sorted.appended(finalElement)
  val clusters = list.foldLeft((List.empty[Cluster], Option.empty[Cluster])) { (state, number) =>
    val (finished, current) = state
    current match {
      case None => (finished, Some(List(number)))
      case Some(cluster) if (number - cluster.max >= 3) => (finished :+ cluster, Some(List(number)))
      case growingCluster => (finished, growingCluster.map(_.added(number)))
    }
  }._1.filter(_.size >= 3)  // Clusters of less elements count as neutral: 1
  clusters.map(c => c -> variantRec(c.tail, List(c.head)).size).toMap
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val numbers = Input.asLines().map(_.toInt)
    val summary = calculateDifferences(numbers)
    println(s"Differences -- Ones: ${summary.ones} / Threes: ${summary.threes} => ${summary.ones * summary.threes}")
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val numbers = Input.asLines().map(_.toInt)
    val clusters = calculateNumberOfVariants(numbers)
    val total = clusters.values.foldLeft(1L)(_ * _)
    println(s"Identified ${clusters.size} clusters with a total variant count of $total")
  }
}