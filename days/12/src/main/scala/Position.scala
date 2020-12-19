import Math.abs

case class Position(x: Int, y: Int) {
  def move(dir: Direction, count: Int = 1): Position = dir.move(this, count)
  def distance(that: Position = Position.origin): Int = abs(this.x - that.x) + abs(this.y - that.y )
}

object Position {
  val origin = Position(0, 0)
}