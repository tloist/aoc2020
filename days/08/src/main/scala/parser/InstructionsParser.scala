package parser

import common._
import cats._
import cats.data._
import cats.implicits._

object InstructionsParser {
  
  enum Error {
    case UnknownInstruction(operation: String)
    case UnknownOpCode(code: String)
    case IllegalStructuredLine(line: String)
    
    def errorMessage = this match
      case IllegalStructuredLine(line) => s"Line is not structured correctly: '$line'"
      case UnknownOpCode(unknown) => s"Unknown OpCode found '$unknown'"
      case UnknownInstruction(operation) => s"Unknown operation: '$operation'!"
  }
  
  def apply(input: String): Either[Error, List[Instruction]] = input.split("\n").toList.traverse(readLine)
  
  private def readLine(line: String): Either[Error, Instruction] = {
    val splitted = line.split(" ")
    if splitted.size != 2 then Left(Error.IllegalStructuredLine(line))
    else OpCode.values.find(_.code == splitted(0))
      .map(_.toInstruction(splitted(1).toInt))
      .map(Right(_))
      .getOrElse(Left(Error.UnknownOpCode(splitted(0))))
  }
}
