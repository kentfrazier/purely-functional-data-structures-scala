package com.kentfrazier.purefunc.numeric

import scala.annotation.tailrec

/**
 * Sparse representation of binary numbers, with digits in increasing order of significance
 *
 * See Chapter 9.2
 */
object SparseBinaryNumber {

  /**
   * List of weights, each of which is a monotonically increasing power of two
   */
  type Nat = List[Int]
  object Nat {
    implicit case object SparseBinaryNumberBN extends BinaryNumber[Nat] {

      @tailrec
      def carry(weight: Int, n: Nat): Nat = n match {
        case Nil =>
          weight :: Nil
        case current @ head :: rest =>
          if (weight < head) {
            weight :: current
          } else {
            carry(weight * 2, rest)
          }
      }

      def borrow(weight: Int, n: Nat): Nat = n match {
        case Nil =>
          throw BinaryNumber.IllegalSubtractionError
        case current @ head :: rest =>
          if (weight == head) {
            rest
          } else {
            weight :: borrow(weight * 2, current)
          }
      }

      override def increment(n: Nat): Nat = carry(1, n)

      override def decrement(n: Nat): Nat = borrow(1, n)

      override val zero: Nat = Nil

      override def toInt(n: Nat): Int = n.sum

      override def fromInt(i: Int): Nat = {
        BinaryNumber.oneBitRanks(i)
          .map(1 << _)
          .sorted
      }

    }
  }
}