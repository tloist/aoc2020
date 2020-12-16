import better.files.Resource
import parser.InstructionsParser
import common.Program
import common.ExecutionResult._

class Example1Tests extends munit.FunSuite {
  
  test("Check parsing the example program works") {
    import common.Instruction._
    InstructionsParser(Resource.getAsString("example1.txt")) match
      case Left(error) => fail(s"Parsing instructions failed due to: $error")
      case Right(instructions) => assertEquals(instructions, List(
        NoOperation(0),
        ModifyAccumulator(1),
        Jump(4),
        ModifyAccumulator(3),
        Jump(-3),
        ModifyAccumulator(-99),
        ModifyAccumulator(1),
        Jump(-4),
        ModifyAccumulator(6)
      ))
  }

  test("Check program stops expected") {
    import common.Instruction._
    InstructionsParser(Resource.getAsString("example1.txt"))
      .map(Program(_))
      .map(_.run.last)
      .fold(error => fail(s"Parsing instructions failed: $error"), {
        case Running(program) => assertEquals(program.acc, 5)
        case other => fail(s"Unexpected program terminiation: $other")
      })
  }
  
}
