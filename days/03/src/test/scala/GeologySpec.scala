class GeologySpec extends munit.FunSuite {
  val geo = Geology.parseFrom("""|..##.......
                                 |#...#...#..
                                 |.#....#..#.
                                 |..#.#...#.#
                                 |.#...##..#.
                                 |..#.##.....
                                 |.#.#.#....#
                                 |.#........#
                                 |#.##...#...
                                 |#...##....#
                                 |.#..#...#.#""".stripMargin)

  test("Example input has the right map size") {
    assertEquals(geo.width, 11)
    assertEquals(geo.height, 11)
  }
  
  test("Example input has trees at the right spots") {
    List((2,0), (3,0), (0,1), (4,1), (8,1)).map(Coordinate.apply).foreach { c =>
      assertEquals(geo.fieldAt(c), Some(Field.Tree))
    }
  }
  
  test("Tracing through the geology with the example move hits 7 trees") {
    assertEquals(geo.traceOrigin(Move(3,1)).count(_ == Field.Tree), 7)
  }
  
}
