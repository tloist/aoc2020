package customs

opaque type BagName = String

object BagName {
  def apply(input: String): BagName = input
  
  given Ordering[BagName] = Ordering.String
}

case class QuantityBag(name: BagName, quantity: Int) {
  override def toString: String = s"${quantity}x '$name'"
}

case class Rule(bag: BagName, contains: Set[QuantityBag]) {
  val textual = s"'$bag' may contain " +
    contains.toList.sortBy(_._1).map(bag => s"${bag.quantity}x ${bag.name}").mkString(" and ")
  override def toString: String = textual
}