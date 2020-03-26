package com.kentfrazier.purefunc.numeric

/**
 * Type-class trait for implementing binary number operations on different binary numeric representations.
 *
 * This is in support of all the stuff in Chapter 9 of the book.
 */
trait BinaryNumber[N] {

  def increment(n: N): N

  def decrement(n: N): N

  def toInt(n: N): Int

  val zero: N

  /**
   * Create an instance of N representing the natural number i
   *
   * This default implementation is very inefficient and so concrete implementations should override it with
   * something more efficient if possible.
   *
   * @param i an Int representation of the target number
   * @return that number, encoded as an N
   */
  def fromInt(i: Int): N = {
    require(i >= 0, "Only non-negative integers can be represented")
    (0 until i).foldLeft(zero) {
      case (n, _) =>
        increment(n)
    }
  }

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