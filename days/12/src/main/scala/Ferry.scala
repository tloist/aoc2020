import Direction._
import Instruction._

case class Ferry(position: Position = Position.origin, heading: Direction = East) {
  
  def execute(instr: Instruction): Ferry = instr match {
    case MoveNorth(steps) => copy(position = position.move(North, steps))
    case MoveEast(steps) => copy(position = position.move(East, steps))
    case MoveSouth(steps) => copy(position = position.move(South, steps))
    case MoveWest(steps) => copy(position = position.move(West, steps))
    case Forward(steps) => copy(position = position.move(heading, steps))
    case TurnLeft(degree) => copy(heading = heading.turn(Turn.Left, degree))
    case TurnRight(degree) => copy(heading = heading.turn(Turn.Right, degree))
  }
  
  def execute(instructions: List[Instruction]): Ferry =
    instructions.foldLeft(this)(_.execute(_))
  
}
