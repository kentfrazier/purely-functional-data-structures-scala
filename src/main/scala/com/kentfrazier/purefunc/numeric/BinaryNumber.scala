package com.kentfrazier.purefunc.numeric

/**
 * Type-class trait for implementing binary number operations on different binary numeric representations.
 *
 * This is in support of all the stuff in Chapter 9 of the book.
 */
trait BinaryNumber[N] {

  def increment(n: N): N

  def decrement(n: N): N

  def add(n1: N, n2: N): N

  val zero: N

  lazy val one: N = increment(zero)

  /**
   * Create an instance of N representing the natural number i
   *
   * This default implementation is pretty inefficient (O(log2(i))) and so concrete implementations should override it
   * with something more efficient if possible.
   *
   * @param i an Int representation of the target number
   * @return that number, encoded as an N
   */
  def fromInt(i: Int): N = {
    require(i >= 0, "Only non-negative integers can be represented")
    if (i == 0) {
      zero
    } else {
      val halfI = i >> 1
      val halfN = fromInt(halfI)
      if ((i & 1) == 1) {
        increment(add(halfN, halfN))
      } else {
        add(halfN, halfN)
      }
    }
  }

  def toInt(n: N): Int

}
object BinaryNumber {

  def apply[N : BinaryNumber]: BinaryNumber[N] = implicitly[BinaryNumber[N]]

  case object IllegalSubtractionError extends IllegalStateException

  def oneBitRanks(i: Int): List[Int] = {
    Iterator.from(0)
      .map { rank =>
        val current = i >> rank
        val isOneBit = (current & 1) == 1
        (rank, isOneBit, current)
      }
      .takeWhile(_._3 > 0)
      .collect {
        case (rank, isOneBit, _) if isOneBit =>
          rank
      }
      .toList
  }

}