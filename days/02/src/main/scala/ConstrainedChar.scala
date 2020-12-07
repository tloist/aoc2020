case class ConstrainedChar(char: Char, c1: Int, c2: Int) {
  override def toString = s"Char['$char' $c1/$c2]"
}

trait PasswordPolicy {
  def validate(password: String, constrain: ConstrainedChar): Boolean
}