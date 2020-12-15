package graph

import better.files.Resource
import customs.{BagName, QuantityBag, Rule}
import parser.RulesParser

object Bags {
  val shinyGold = BagName("shiny gold")
  val fadedBlue = BagName("faded blue")
  val darkOrange = BagName("dark orange")
  val lightRed = BagName("light red")
  val mutedYellow = BagName("muted yellow")
  val brightWhite = BagName("bright white")  
}

class GraphTests extends munit.FunSuite {
  import Bags._

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
  
}