package common

enum OpCode(val code: String) {
  case NoOperation extends OpCode("nop")
  case ModifyAccumulator extends OpCode("acc")
  case Jump extends OpCode("jmp")
  
  def toInstruction(param: Int): Instruction = Instruction.fromOpCode(this, param)
}

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
  
}

enum ExecutionResult {
  case InfiniteLoopDetected(finalState: Program)
  case Running(program: Program)
  case RanOutOfInstruction(list: List[Instruction], ip: Int, previousIp: Int)
  
  def modifyRunning(func: Program => Program): ExecutionResult = this match {
    case Running(program) => Running(func(program))
    case other => other
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
  
  def run: LazyList[ExecutionResult] = LazyList.unfold(Running(this)) {
    case running as Running(program) => Option(running, program.step)
    case _ => None
  }
  
  def currentInstruction: Instruction = instructions(ip)
  override def toString: String = s"Program(IP: $ip '$currentInstruction'/ ${instructions.size}x instr. | Acc: $acc | Visited: $visited)"
  
  private[this] def verify(mod: Modification): ExecutionResult = {
    val future = mod(this)
    if (visited(future.ip)) then InfiniteLoopDetected(this)
    else if (future.ip < 0 || future.ip >= instructions.size) RanOutOfInstruction(instructions, future.ip, ip)
    else Running(future)
  }
  
  private[this] def increaseIp(modifier: Int = +1): Modification = _.copy(ip = ip + modifier, visited = visited + ip)
  private[this] def increaseAcc(modifier: Int = +1): Modification = _.copy(acc = acc + modifier)
}
