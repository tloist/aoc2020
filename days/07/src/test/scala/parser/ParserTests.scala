package parser

import better.files.Resource
import cats.data._
import customs._
import parser.RulesParser.{readline => parseLine}

class ParserTests extends munit.FunSuite {
  
  def checkParseLine(name: String)(input: String, bagName: String, content: Map[String, Int])(using loc: munit.Location): Unit =
    test(name) {
      val qtyBag: Set[QuantityBag] = content.map((name, qty) => QuantityBag(BagName(name), qty)).toSet
      parseLine(input) match {
        case Left(error) => fail(s"Parsing error: '$error' while parsing input: \n$input")
        case Right(result) => assertEquals(result, Rule(BagName(bagName), qtyBag))
      }
    }
  
  checkParseLine("Parse example line #1: light red")(
    "light red bags contain 1 bright white bag, 2 muted yellow bags.", 
    "light red", Map(
    "bright white" -> 1,
    "muted yellow" -> 2
  ))

  checkParseLine("Parse example line #2: dark orange")(
  "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
  "dark orange", Map(
    "bright white" -> 3,
    "muted yellow" -> 4
  ))

  checkParseLine("Parse example line #3: bright white")(
  "bright white bags contain 1 shiny gold bag.",
  "bright white", Map(
    "shiny gold" -> 1
  ))

  checkParseLine("Parse example line #4: muted yellow")(
  "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
  "muted yellow", Map(
    "shiny gold" -> 2,
    "faded blue" -> 9
  ))

  checkParseLine("Parse example line #5: shiny gold")(
  "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
  "shiny gold", Map(
    "dark olive" -> 1,
    "vibrant plum" -> 2
  ))

  checkParseLine("Parse example line #6: dark olive")(
  "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
  "dark olive", Map(
    "faded blue" -> 3,
    "dotted black" -> 4
  ))

  checkParseLine("Parse example line #7: vibrant plum")(
  "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
  "vibrant plum", Map(
    "faded blue" -> 5,
    "dotted black" -> 6
  ))

  checkParseLine("Parse example line #8: faded blue")(
  "faded blue bags contain no other bags.",
  "faded blue", Map.empty)

  checkParseLine("Parse example line #9: dotted black")(
    "dotted black bags contain no other bags.",
    "dotted black", Map.empty)
  
  test("Complete example") {
    parser.RulesParser(Resource.getAsString("exampleA.txt")) match {
      case Left(error) => fail(s"Parsin failed due to $error")
      case Right(rules) => assertEquals(rules.size, 9)
    }
  }
}
