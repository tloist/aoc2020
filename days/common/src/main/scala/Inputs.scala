object Input {
  import better.files._
  import java.io.InputStream

  def asStream(part: TaskPart): InputStream = Resource.getAsStream(part.inputFileName)
  def asString(part: TaskPart): String = Resource.getAsString(part.inputFileName)
  def asLines(part: TaskPart): List[String] = asString(part).split('\n').toList
}
