import SpecPath._
import SpecPath.SpecDimension._

class SpecPathTests extends munit.FunSuite {
  
  test("Can't overspecify something") {
    val total = SpecPath(1,1,1,1)
    def overspecifiedWith(step: Step) = Left(Error.Overspecification(total.copy(specs = total.specs :+ step)))

    assertEquals(total.step(Step.Front), overspecifiedWith(Step.Front))
    assertEquals(total.step(Step.Back), overspecifiedWith(Step.Back))
    assertEquals(total.step(Step.Left), overspecifiedWith(Step.Left))
    assertEquals(total.step(Step.Right), overspecifiedWith(Step.Right))
  }

  test("Example character spec: Step #1") {
    assertSuccess(SpecPath(0, 127, 0, 7).step(Step.Front)) { spec =>
      assertEquals(spec.colMin, 0)
      assertEquals(spec.colMax, 63)
      assertEquals(spec.rowMin, 0)
      assertEquals(spec.rowMax, 7)
      assertEquals(spec.specs, List(Step.Front))
      assert(!spec.isSpecified)
      assert(spec.seatId.isLeft)
    }
  }

  test("Example character spec: Step #2") {
    assertSuccess(SpecPath(0, 63, 0, 7).step(Step.Back)) { spec =>
      assertEquals(spec.colMin, 32)
      assertEquals(spec.colMax, 63)
      assertEquals(spec.rowMin, 0)
      assertEquals(spec.rowMax, 7)
      assertEquals(spec.specs, List(Step.Back))
    }
  }

  test("Example character spec: Step #3") {
    assertSuccess(SpecPath(32, 63, 0, 7).step(Step.Front)) { spec =>
      assertEquals(spec.colMin, 32)
      assertEquals(spec.colMax, 47)
    }
  }

  test("Example character spec: Step #4") {
    assertSuccess(SpecPath(32, 47, 0, 7).step(Step.Back)) { spec =>
      assertEquals(spec.colMin, 40)
      assertEquals(spec.colMax, 47)
    }
  }

  test("Example character spec: Step #5") {
    assertSuccess(SpecPath(40, 47, 0, 7).step(Step.Back)) { spec =>
      assertEquals(spec.colMin, 44)
      assertEquals(spec.colMax, 47)
    }
  }

  test("Example character spec: Step #6") {
    assertSuccess(SpecPath(44, 47, 0, 7).step(Step.Front)) { spec =>
      assertEquals(spec.colMin, 44)
      assertEquals(spec.colMax, 45)
    }
  }

  test("Example character spec: Step #7") {
    assertSuccess(SpecPath(44, 45, 0, 7).step(Step.Front)) { spec =>
      assertEquals(spec.colMin, 44)
      assertEquals(spec.colMax, 44)
      assert(spec.isSpecified(Column))
    }
  }

  test("Example character spec: Step #8") {
    assertSuccess(SpecPath(44, 44, 0, 7).step(Step.Right)) { spec =>
      assertEquals(spec.colMin, 44)
      assertEquals(spec.colMax, 44)
      assertEquals(spec.rowMin, 4)
      assertEquals(spec.rowMax, 7)
      assert(spec.isSpecified(Column))
    }
  }

  test("Example character spec: Step #9") {
    assertSuccess(SpecPath(44, 44, 4, 7).step(Step.Left)) { spec =>
      assertEquals(spec.colMin, 44)
      assertEquals(spec.colMax, 44)
      assertEquals(spec.rowMin, 4)
      assertEquals(spec.rowMax, 5)
      assert(spec.isSpecified(Column))
    }
  }

  test("Example character spec: Step #10") {
    assertSuccess(SpecPath(44, 44, 4, 5).step(Step.Right)) { spec =>
      assertEquals(spec.colMin, 44)
      assertEquals(spec.colMax, 44)
      assertEquals(spec.rowMin, 5)
      assertEquals(spec.rowMax, 5)
      assert(spec.isSpecified(Column))
      assert(spec.isSpecified(Row))
      assert(spec.isSpecified)
      assertEquals(spec.seatId, Right(357))
    }
  }
  
  def assertSuccess(result: Either[Error, SpecPath])(verification: SpecPath => Unit): Unit = result match
    case Right(spec) => verification(spec)
    case Left(error) => fail(error.errorMessage)
  
}
