import cats._
import cats.data._
import cats.implicits._

def calculateSeat(initial: SpecPath, steps: NonEmptyList[SpecPath.Step]): Either[SpecPath.Error, SpecPath] =
  steps.foldLeft(Right(initial).withLeft[SpecPath.Error]){ (state, current) => state.flatMap(_.step(current)) }

def parseOrException(input: String) = SeatSpecificationParser(input).left.map(error => throw new IllegalArgumentException(s"Parsing errors: $error")).toTry.get

object Part1 {
  val airplane = SpecPath(0, 127, 0, 7)
  
  def main(args: Array[String]): Unit = {
    val input = Input.asString(TaskPart.First);
    val specs: List[NonEmptyList[SpecPath.Step]] = parseOrException(input).toList
    val result = for {
      spec <- specs
      path <- calculateSeat(airplane, spec).toSeq
      seatId <- path.seatId.toSeq
    } yield (path, seatId)
    val (path, seatId) = result.maxBy(_._2)
    println(s"Seat ID $seatId on row ${path.rowMin} and column ${path.colMin}")
  }
  
}
