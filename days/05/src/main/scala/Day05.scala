import cats._
import cats.data._
import cats.implicits._

given GridSeat.Dimension = GridSeat.Dimension(0, 7, 0, 127)

def calculateSeat(steps: NonEmptyList[SpecPath.Step]): Either[SpecPath.Error, SpecPath] =
  steps.foldLeft(Right(SpecPath.create).withLeft[SpecPath.Error]){ (state, current) => state.flatMap(_.step(current)) }

def parseOrException(input: String): List[NonEmptyList[SpecPath.Step]] = SeatSpecificationParser(input)
  .left.map(error => throw new IllegalArgumentException(s"Parsing errors: $error"))
  .toTry.get.toList

def parseSpecsAndSeatIds(input: String): List[GridSeat] = for {
  spec <- parseOrException(input)
  path <- calculateSeat(spec).toSeq
  seat <- path.toGridSeat.toSeq
} yield seat
  

object Part1 {
  def main(args: Array[String]): Unit = {
    val max = parseSpecsAndSeatIds(Input.asString(TaskPart.First)).maxBy(_.seatId)
    println(s"Seat ID ${max.seatId} on row ${max.row} and column ${max.column}")
  }
}

object Part2 {
  def notVeryFrontOrBack(using dimension: GridSeat.Dimension): GridSeat => Boolean = seat =>
    seat.row != dimension.rowMin && seat.row != dimension.rowMax
  
  def main(args: Array[String]): Unit = {
    val occupied = parseSpecsAndSeatIds(Input.asString(TaskPart.First)).toSet
    val candidates = GridSeat.all
      .filter(notVeryFrontOrBack)
      .filter(!occupied(_))
      .filter(_.neighbors.forall(occupied(_)))
    candidates.foreach(seat => println(s"Remaining seat is row: ${seat.row} column: ${seat.column} with ID: ${seat.seatId}"))
    
  }
}
