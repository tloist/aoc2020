enum Instruction(val char: Char){
  case MoveNorth(steps: Int) extends Instruction('N')
  case MoveEast(steps: Int) extends Instruction('E')
  case MoveSouth(steps: Int) extends Instruction('S')
  case MoveWest(steps: Int) extends Instruction('W')
  case Forward(steps: Int) extends Instruction('F')
  case TurnLeft(degree: Int) extends Instruction('L')
  case TurnRight(degree: Int) extends Instruction('R')
}

object Instruction {
  def fromLine(input: String): Instruction =
    val value = input.tail.toInt
    input.head match
      case 'N' => MoveNorth(value)
      case 'E' => MoveEast(value)
      case 'S' => MoveSouth(value)
      case 'W' => MoveWest(value)
      case 'F' => Forward(value)
      case 'L' => TurnLeft(value)
      case 'R' => TurnRight(value)
}