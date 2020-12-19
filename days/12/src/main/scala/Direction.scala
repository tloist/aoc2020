enum Turn {
  case Left, Right
  
  def opposite: Turn = this match {
    case Left => Right
    case Right => Left
  }
}

enum Direction() {
  case North, East, South, West
  
  def move(pos: Position, count: Int = 1): Position = this match
    case North => Position(pos.x, pos.y - count)
    case East => Position(pos.x + count, pos.y)
    case South => Position(pos.x, pos.y + count)
    case West => Position(pos.x - count, pos.y)
  
  def turn(turn: Turn, degree: Int = 90): Direction = {
    val rotation = turn match
      case Turn.Left => leftRotation
      case Turn.Right => rightRotation
    val offset = rotation.indexOf(this)
    val steps = (degree / 90) % 360
    rotation((steps + offset) % rotation.length)
  }

  private lazy val rightRotation = List(North, East, South, West)
  private lazy val leftRotation = List(North, West, South, East)
}

