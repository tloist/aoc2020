import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen._
import Direction._
import Turn._

class DirectionTests extends ScalaCheckSuite {
  opaque type Degree = Int
  given Arbitrary[Direction] = Arbitrary(oneOf(Direction.values.toList))
  given Arbitrary[Turn] = Arbitrary(oneOf(Turn.values.toList))
  given Arbitrary[Degree] = Arbitrary(oneOf(90, 180, 270))
  
  property("Turning in one direction and turning the other neutralizes itself") {
    forAll { (dir: Direction, turn: Turn) =>
      dir.turn(turn).turn(turn.opposite) == dir
    }
  }
  
  property("Turning 270 is the same as turning the other way around 90 degree") {
    forAll { (dir: Direction, turn: Turn) =>
      dir.turn(turn, 90) == dir.turn(turn.opposite, 270)
    }
  }
  
  property("Turning 180 is its own inverse operation") {
    forAll { (dir: Direction, turn: Turn) =>
      dir.turn(turn, 180).turn(turn, 180) == dir
    }
  }

}