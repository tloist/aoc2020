package common

enum Instruction {
  case NoOperation(ignored: Int)
  case ModifyAccumulator(modifier: Int)
  case Jump(offset: Int)
  
  def param: Int = this match
    case NoOperation(ignored) => ignored
    case ModifyAccumulator(modifier) => modifier
    case Jump(offset) => offset
}

object Instruction {
  
  def fromOpCode(code: OpCode, param: Int): Instruction = code match {
    case OpCode.NoOperation => Instruction.NoOperation(param.toInt)
    case OpCode.ModifyAccumulator => Instruction.ModifyAccumulator(param.toInt)
    case OpCode.Jump => Instruction.Jump(param.toInt)
  }

  enum OpCode(val code: String) {
    case NoOperation extends OpCode("nop")
    case ModifyAccumulator extends OpCode("acc")
    case Jump extends OpCode("jmp")

    def toInstruction(param: Int): Instruction = Instruction.fromOpCode(this, param)
  }
}

enum ExecutionResult {
  case InfiniteLoopDetected(before: Program, looped: Program)
  case Running(program: Program)
  case RanOutOfInstruction(before: Program, futureIp: Int)
  
  def modifyRunning(func: Program => Program): ExecutionResult = this match {
    case Running(program) => Running(func(program))
    case other => other
  }
  
  override def toString: String = this match {
    case InfiniteLoopDetected(before, looped) => 
      s"""InfiniteLoopDetected(looped=$looped)
         |                     before=$before""".stripMargin
    case Running(program) => s"Running $program"
    case RanOutOfInstruction(before, futureIp) => s"Out of instructions at ${futureIp + 1} where previously it was ${before.ip + 1}"
  }
}

case class Program(instructions: List[Instruction], val acc: Int = 0, val ip: Int = 0, visited: Set[Int] = Set.empty) {
  import Instruction._
  import ExecutionResult._
  
  type Modification = Program => Program
  
  def step: ExecutionResult = currentInstruction match {
    case NoOperation(param) => verify(increaseIp())
    case ModifyAccumulator(modifier) => verify(increaseIp() andThen increaseAcc(modifier))
    case Jump(distance) => verify(increaseIp(distance))
  }
  
  def run: LazyList[ExecutionResult] = Running(this) #:: LazyList.unfold(Running(this)) {
    case running as Running(program) => Option(program.step, program.step)
    case _ => None
  }
  
  def currentInstruction: Instruction = instructions(ip)
  override def toString: String = s"Program(IP: ${ip + 1} of ${instructions.size}x instr.: $currentInstruction | " +
    s"Acc: $acc | Visited: [${visited.toList.sorted.mkString(", ")}])"
  
  private[this] def verify(mod: Modification): ExecutionResult = {
    val future = mod(this)
    if (visited(future.ip)) then InfiniteLoopDetected(this, future)
    else if (future.ip < 0 || future.ip >= instructions.size) RanOutOfInstruction(this, future.ip)
    else Running(future)
  }
  
  private[this] def increaseIp(modifier: Int = +1): Modification = _.copy(ip = ip + modifier, visited = visited + ip)
  private[this] def increaseAcc(modifier: Int = +1): Modification = _.copy(acc = acc + modifier)
}

extension (program: Program) {
  
  def updatedInstruction(pos: Int, update: Instruction): Program = program.copy(instructions = program.instructions.updated(pos, update))
  
  def exchangeJumpAndNoOps: List[Program] = program.instructions.zipWithIndex.collect {
    case (Instruction.NoOperation(param), idx) => (idx, Instruction.Jump(param))
    case (Instruction.Jump(param), idx) => (idx, Instruction.NoOperation(param))
  }.map(program.updatedInstruction(_, _))
  
  def findFixedProgram: List[Program] = program.exchangeJumpAndNoOps
    .map(_.run.last)
    .collect {
      case ExecutionResult.RanOutOfInstruction(before, futureIp) => before
    }
}