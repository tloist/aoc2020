import customs.BagName
import parser.RulesParser
import graph.Graph

val shinyGold = BagName("shiny gold")

object Part1 {
  def main(args: Array[String]): Unit = {
    RulesParser(Input.asString()) match {
      case Left(error) => println(s"Couldn't parse the input due to: $error")
      case Right(rules) =>
        val graph = Graph(rules)
        val preds = graph.allPredecessorsFrom(shinyGold)
        println(s"${preds.size} predecessors found: ${preds.map(_.toString).toList.sorted.mkString(", ")}")
    }
  }
}