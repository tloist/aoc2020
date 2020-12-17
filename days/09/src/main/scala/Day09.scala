import better.files.Resource
import xmascrypt._

object Part1 {
  def main(args: Array[String]): Unit = {
    val numbers = Input.asLines().map(_.toLong).toList
    xmasVerify(25)(numbers).find(!_.isValid).foreach(res => println(s"${res.number} is the first invalid number"))
  }
}