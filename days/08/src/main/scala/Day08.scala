import parser.InstructionsParser
import common.Program

object Part1 {
  def main(args: Array[String]): Unit = {
    InstructionsParser(Input.asString()).map(Program(_)) match {
      case Left(error) => println(s"Error occured: $error")
      case Right(program) => program.run.foreach(println)
    }
  }
}
