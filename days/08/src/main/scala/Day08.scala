import parser.InstructionsParser
import common._
import common.Instruction._

object Part1 {
  def main(args: Array[String]): Unit = {
    InstructionsParser(Input.asString()).map(Program(_)) match {
      case Left(error) => println(s"Error occured: $error")
      case Right(program) => program.run.foreach(println)
    }
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    InstructionsParser(Input.asString()).map(Program(_)).map(_.findFixedProgram).foreach(println)
  }
}