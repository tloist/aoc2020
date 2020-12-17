package xmascrypt

import math.Ordered.orderingToOrdered
import math.Ordering.Implicits.infixOrderingOps

enum VerificationResult[N] {
  case Valid(number: N, previous: List[N])
  case Invalid(number: N, previous: List[N])

  def number: N
  def previous: List[N]
  
  def isValid: Boolean = this match
    case Valid(_,_) => true
    case Invalid(_,_) => false
}

def xmasVerify[N: Numeric](blockSize: Int)(numbers: List[N]): List[VerificationResult[N]] =
  numbers.sliding(blockSize + 1).map { slice =>
    if slice.init.combinations(2).map(_.sum).exists(_ == slice.last) then VerificationResult.Valid(slice.last, slice.init)
    else VerificationResult.Invalid(slice.last, slice.init)
  }.toList

def findContiguousSetSumUpTo[N](target: N, numbers: List[N])(using numberOps: Numeric[N]): Option[(N, N)] = {
  import numberOps._
  def prefixUntilSumReached(list: List[N]): List[N] = {
    var sum = zero
    list.takeWhile(nr => {sum += nr; sum <= target})
  }
  numbers.tails
    .map(prefixUntilSumReached)
    .find(_.sum == target)
    .map(found => (found.min, found.max))
}
