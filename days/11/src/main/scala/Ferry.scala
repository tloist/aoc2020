import Ferry.Position.neighborModifier
import Ferry._
import scala.annotation.targetName
import Ferry.PositionState._
import Ferry.SeatState._

object Ferry {
  case class Position(x: Int, y: Int) {
    def modify(modifier: (Int, Int)): Position = Position(x + modifier._1, y + modifier._2)
  }
  
  object Position {
    def neighborModifier: Set[(Int, Int)] = (
      for {
        x <- -1 to +1
        y <- -1 to +1
        if x != 0 || y != 0
      } yield (x, y)
    ).toSet
  }

  enum SeatState(val symbol: Char) {
    case Occupied extends SeatState('#')
    case Empty extends SeatState('L')
  }
  
  enum PositionState {
    case Floor
    case Seat(state: SeatState)
    
    def symbol: Char = this match
      case Floor => '.'
      case Seat(state) => state.symbol
  }
  
  def fromString(input: String): Ferry = { 
    val floor: Map[Position, PositionState] = (for {
      (line, y) <- input.linesIterator.zipWithIndex
      (char, x) <- line.zipWithIndex
      state = char match
        case '.' => PositionState.Floor
        case 'L' => PositionState.Seat(SeatState.Empty)
        case '#' => PositionState.Seat(SeatState.Occupied)
    } yield Position(x,y) -> state).toMap
    val width = floor.keys.map(_.x).max + 1
    val height = floor.keys.map(_.y).max + 1
    val result = Ferry(width, height, floor)
    result.allPositions.find(!floor.isDefinedAt(_)).foreach { pos => 
      throw new IllegalArgumentException(s"Couldn't read position $pos - np input there!")
    }
    result
  }
  
  enum VisionMode(neighborToleranceThreshold: Int) {
    case Direct extends VisionMode(4)
    case Rays extends VisionMode(5)
    
    def neighbors(ferry: Ferry)(origin: Position): Set[Position] = this match {
      case Direct => Position.neighborModifier.map(origin.modify).filter(ferry.contains)
      case Rays => Position.neighborModifier.toList
        .map(modifier => LazyList.iterate(origin.modify(modifier))(_.modify(modifier)))
        .map(_.takeWhile(ferry.contains))   // Stay within the ferry
        .map(_.takeUpTo(ferry(_) == Floor)) // If we hit something other than the floor, just at that space
        .flatMap(_.lastOption)
        .toSet
      }
    
    def evolute(ferry: Ferry)(pos: Position): PositionState = 
      val occupiedCount = neighbors(ferry)(pos).count(ferry(_) == Seat(Occupied))
      ferry(pos) match
        case Seat(Empty) if occupiedCount == 0 => Seat(Occupied)
        case Seat(Occupied) if occupiedCount >= neighborToleranceThreshold => Seat(Empty)
        case other => other
    }

  extension [T] (list: LazyList[T]) {
    def takeUpTo(pred: T => Boolean) =
      var stopOnNext = false
      list.takeWhile { elem =>
        if stopOnNext then false
        else {
          stopOnNext = !pred(elem)
          true
        }
      }
  }
}

case class Ferry(width: Int, height: Int, floor: Map[Position, PositionState]) {
  def allPositions: List[Position] = allPositionLined.flatten
  def countOccupiedSeats: Int = floor.values.count(_ == Seat(Occupied))
  
  def states(using strategy: VisionMode): LazyList[Ferry] = this #:: LazyList.unfold(this) { current =>
    val next = current.copy(floor = current.floor.map((k,_) => k -> strategy.evolute(current)(k)).toMap)
    if next != current then Option((next, next)) else None
  }
  
  def nextState(using strategy: VisionMode): Option[Ferry] = states.tail.headOption

  def contains(pos: Position): Boolean = 
    pos.x >= 0 && pos.x < width &&
    pos.y >= 0 && pos.y < height
    
  export floor.apply

  override def toString: String = allPositionLined.map(_.map(floor(_).symbol).mkString("")).mkString("\n")
  private def allPositionLined: List[List[Position]] =
    (0 until height).map(y => (0 until width).map(x => Position(x,y)).toList).toList
}

