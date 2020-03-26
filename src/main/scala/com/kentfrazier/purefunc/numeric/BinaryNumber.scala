package com.kentfrazier.purefunc.numeric

import scala.util.Try

/**
 * Type-class trait for implementing binary number operations on different binary numeric representations.
 *
 * This is in support of all the stuff in Chapter 9 of the book.
 */
trait BinaryNumber[N] {

  def increment(n: N): N

  def decrement(n: N): N

}
object BinaryNumber {

  case object IllegalSubtractionError extends IllegalStateException

}