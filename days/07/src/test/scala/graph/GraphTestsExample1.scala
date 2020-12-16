package graph

import better.files.Resource
import customs.{BagName, QuantityBag, Rule}
import graph.Bags.shinyGold
import parser.RulesParser

object Bags {
  val brightWhite = BagName("bright white")  
  val darkOrange = BagName("dark orange")
  val darkOlive = BagName("dark olive")
  val dottedBlack = BagName("dotted black")
  val fadedBlue = BagName("faded blue")
  val lightRed = BagName("light red")
  val shinyGold = BagName("shiny gold")
  val mutedYellow = BagName("muted yellow")
  val vibrantPlum = BagName("vibrant plum")
}
import Bags._

class GraphTestsExample1 extends munit.FunSuite {

  val graph = RulesParser(Resource.getAsString("exampleA.txt")).map(Graph.apply).getOrElse {
    throw new IllegalArgumentException("Creation of graph from example failed, see ParserTests suite!")
  }
  
  test("Predecessors for 'shiny gold'") {
    assertEquals(graph.allPredecessorsFrom(shinyGold) , Set(brightWhite, darkOrange, lightRed, mutedYellow))
  }

  test("Predecessors for 'shiny gold': Step #1") {
    assertEquals(graph.predecessorsFrom(shinyGold).head , Set(brightWhite, mutedYellow))
  }

  test("Predecessors for 'shiny gold': Step #2") {
    assertEquals(graph.predecessorsFrom(shinyGold).tail.head , Set(darkOrange, lightRed))
  }
  
  test("Successors for 'shiny gold'") {
    assertEquals(graph.allSuccessorsFrom(shinyGold) , Set(darkOlive, fadedBlue, dottedBlack, vibrantPlum))
  }
  
  test("Example count for how many bags are required in example 1") {
    assertEquals(graph.bagCountToLeafs(shinyGold), 32)
  }
}

class GraphTestsExample2 extends munit.FunSuite {
  val graph = RulesParser(Resource.getAsString("exampleB.txt")).map(Graph.apply).getOrElse {
    throw new IllegalArgumentException("Creation of graph from example failed, see ParserTests suite!")
  }
  
  test("Example count for how many bags are required in example 1") {
    assertEquals(graph.bagCountToLeafs(shinyGold), 126)
  }
  
  checkCount("Linear bags result in the right count in a simple scenario", 2)(
  """shiny gold bags contain 1 dull aqua bag.
    |dull aqua bags contain 1 shiny purple bag.
    |shiny purple bags contain no other bags.""".stripMargin)
  
  checkCount("Linear bags result with doubling in a simple scenario", 6)(
    """shiny gold bags contain 2 dull aqua bags.
      |dull aqua bags contain 2 shiny purple bags.
      |shiny purple bags contain no other bags.""".stripMargin
  )
  
  checkCount("Heavy fan-out scenario", 5)(
    """shiny gold bags contain 1 white bags, 1 orange bag, 1 green bag, 1 pink bag, 1 brown bag.
      |white purple bags contain no other bags.
      |orange purple bags contain no other bags.
      |green purple bags contain no other bags.
      |pink purple bags contain no other bags.
      |brown purple bags contain no other bags.""".stripMargin
  )

  def checkCount(name: String, expected: Int)(input: String)(using loc: munit.Location): Unit = test(name) {
    val graph = RulesParser(input).map(Graph.apply).getOrElse {
      throw new IllegalArgumentException("Creation of graph from example failed, see ParserTests suite!")
    }
    assertEquals(graph.bagCountToLeafs(shinyGold), expected)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val input = """shiny gold bags contain 1 white bags, 1 orange bag, 1 green bag, 1 pink bag, 1 brown bag.
                  |white purple bags contain no other bags.
                  |orange purple bags contain no other bags.
                  |green purple bags contain no other bags.
                  |pink purple bags contain no other bags.
                  |brown purple bags contain no other bags.""".stripMargin
    RulesParser(input).map(Graph.apply)
      .map(_.bagCountToLeafs(shinyGold))
      .foreach(res => println(s"Total result is $res"))
  }
}