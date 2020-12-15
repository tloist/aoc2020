package parser

import cats._
import cats.data._
import cats.implicits._
import customs._

object RulesParser {
  
  enum Error {
    case DefinitionContentSeparatorMissing(separator: String) extends Error
    case DefinitionContentSeparatorAmbigous(separator: String) extends Error
    
    def errorMessage: String = this match {
      case DefinitionContentSeparatorMissing(separator) => s"Missing separator between name and content: '$separator'"
      case DefinitionContentSeparatorAmbigous(separator) => s"Separator '$separator' is ambiguous and occurs multiple time"
    }
  }
  
  def readline(line: String): Either[Error, Rule] = {
    val separator = "bags contain "
    val contentSeparator = ","
    val splitted = line.stripSuffix(".").split(separator)
    if splitted.size == 0 then Left(Error.DefinitionContentSeparatorMissing(separator))
    else if (splitted.size != 2) then Left(Error.DefinitionContentSeparatorAmbigous(separator))
    else
      def cleanupContent(part: String): String = part.strip().stripSuffix(" bag").stripSuffix(" bags")
      def stringToQuantityBag(part: String): QuantityBag =
        val noPart = part.takeWhile(_.isDigit)
        QuantityBag(BagName(part.stripPrefix(noPart).strip()), noPart.toInt)
      val bags =
        if("no other bags" == splitted(1)) then Set.empty
        else splitted(1).split(contentSeparator).map(cleanupContent).map(stringToQuantityBag).toSet
      Right(Rule(BagName(splitted(0).strip()), bags))
  }
  
  def apply(input: String): Either[Error, Set[Rule]] = input.split("\n").toList.traverse(readline).map(_.toSet)

}
