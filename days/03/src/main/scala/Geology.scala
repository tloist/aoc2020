import scala.annotation.targetName

enum Field(val char: Char) {
  case Empty extends Field('.')
  case Tree extends Field('#')

  override def toString: String = char.toString
}
object Field {
  def from(origin: Char) = values.find(_.char == origin).getOrElse {
    throw new IllegalArgumentException(s"Illegal character for Field '$origin'!")
  }
}

case class Coordinate(x: Int, y: Int) {
  @targetName("add")
  infix def +(move: Move): Coordinate = Coordinate(x + move.x, y + move.y)
}
object Coordinate {
  val origin = Coordinate(0, 0)
}
case class Move(x: Int, y: Int)

class Geology(val width: Int, val height: Int, map: Map[Coordinate, Field]) {
  
  def fieldAt(x: Int, y: Int): Option[Field] = map.get(Coordinate(x % width, y))
  def fieldAt(coord: Coordinate): Option[Field] = fieldAt(coord.x, coord.y)
  
  def trace(origin: Coordinate, step: Move): LazyList[Field] = LazyList.unfold(origin) { current =>
    fieldAt(current + step).map((_, current + step))
  }
  
  def traceOrigin(step: Move): LazyList[Field] = trace(Coordinate.origin, step)
  
  def mapAsString: String = (
    for {
      y <- (0 until width).toList
      coordinates = (0 until width).map(Coordinate(_, y)).map(map)
    } yield coordinates.map(_.toString).mkString
    ).mkString("\n")
  override def toString = s"Geology[$width/$height]:\n$mapAsString"
}

object Geology {

  def parseFrom(input: String): Geology = {
    val lines = input.split("\n")
    val widths = lines.map(_.length).toSet
    if (widths.size == 1) {
      val fields = for {
        (line, y) <- lines.zipWithIndex
        (char, x) <- line.zipWithIndex
      } yield (Coordinate(x,y), Field.from(char))
      Geology(widths.head, lines.size, fields.toMap)  
    } else throw new IllegalArgumentException(s"Lines have different length: ${widths.mkString(", ")}!")
  }

}
