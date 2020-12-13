import SpecPath.Step
import cats._
import cats.data.{NonEmptyMap, _}
import cats.implicits._
import cats.parse.{Numbers, Parser, Parser1, Rfc5234 => Rfc}

object SeatSpecificationParser {
  
  val stepParser: Parser1[Step] = {
    import SpecPath.Step._
    Parser.oneOf1(List(
      Parser.char('R').void.map(_ => Right),
      Parser.char('L').void.map(_ => Left),
      Parser.char('F').void.map(_ => Front),
      Parser.char('B').void.map(_ => Back),
    ))
  }
  
  val lineParser: Parser1[NonEmptyList[Step]] = stepParser.rep1 <* Parser.char('\n').?
  
  val linesParser: Parser1[NonEmptyList[NonEmptyList[Step]]] = lineParser.rep1
  
  def apply(input: String) = linesParser.parseAll(input)
    
}
