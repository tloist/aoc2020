import Ferry.Position
import better.files.Resource
import Ferry._
import Ferry.PositionState._
import Ferry.SeatState._

class Example1Tests extends munit.FunSuite {
  given stratey as Ferry.VisionMode = Ferry.VisionMode.Direct
  
  test("Occupied seat becomes empty") {
    val origin = Ferry.fromString("""###
                                    |###""".stripMargin)
    val expected = Ferry.fromString("""#L#
                                      |#L#""".stripMargin)
    assert(origin.nextState.map { result =>
      assertEquals(result, expected)
    }.isDefined)
  }
  
  test("Corner case from example becomes empty") {
    val origin = Ferry.fromString(""".##
                                    |.##
                                    |#..""".stripMargin)
    val expected = Ferry.fromString(""".##
                                      |.L#
                                      |#..""".stripMargin)
    assert(origin.nextState.map { result =>
      assertEquals(result, expected)
    }.isDefined)
  }
  
  test("Final state with direct vision has 37 occupied seats") {
    val seats = Ferry.fromString(Resource.getAsString("example1.txt")).states.last.countOccupiedSeats
    assertEquals(seats, 37)
  }
}

class Example2Tests extends munit.FunSuite {
  given strategy as Ferry.VisionMode = Ferry.VisionMode.Rays

  test("Empty seat blocks vision") {
    val ferry = Ferry.fromString(Resource.getAsString("seatBlockVision.txt"))
    val seen = strategy.neighbors(ferry)(Position(1,1)).toList.map(ferry.apply)
    assertEquals(seen.count(_ == Floor), 7)
    assertEquals(seen.count(_ == Seat(Empty)), 1)
  }
  
  test("Central isolated chair sees only floor") {
    val ferry = Ferry.fromString(Resource.getAsString("centralIsolatedChair.txt"))
    val seen = strategy.neighbors(ferry)(Position(3,3)).map(ferry.apply)
    assertEquals(seen, Set(Floor))
  }

  test("Final state with ray vision has 26 occupied seats") {
    val seats = Ferry.fromString(Resource.getAsString("example1.txt")).states.last.countOccupiedSeats
    assertEquals(seats, 26)
  }
}

object Example2Tests {
  given strategy as Ferry.VisionMode = Ferry.VisionMode.Rays
  def main(args: Array[String]): Unit = {
    val ferry = Ferry.fromString(Resource.getAsString("lonelyEmptySeat.txt"))
    println(ferry)
    

    val lonelySeatPos = Position(3,4)
    println(s"Neighbors:")
    println(strategy.neighbors(ferry)(lonelySeatPos).toList.map(ferry(_)))
    println(s"Next state:")
    println(strategy.evolute(ferry)(lonelySeatPos))
  }
}