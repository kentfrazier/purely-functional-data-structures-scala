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
        case current @ head :: tail =>
          if (weight < head) {
            weight :: current
          } else {
            carry(weight * 2, tail)
          }
      }

      def borrow(weight: Int, n: Nat): Nat = n match {
        case Nil =>
          throw BinaryNumber.IllegalSubtractionError
        case current @ head :: tail =>
          if (weight == head) {
            tail
          } else {
            weight :: borrow(weight * 2, current)
          }
      }

      override def increment(n: Nat): Nat = carry(1, n)

      override def decrement(n: Nat): Nat = borrow(1, n)

      override def add(n1: Nat, n2: Nat): Nat = (n1, n2) match {
        case (Nil, other) =>
          other
        case (other, Nil) =>
          other
        case (a @ head1 :: tail1, b @ head2 :: tail2) =>
          if (head1 < head2) {
            head1 :: add(tail1, b)
          } else if (head1 > head2) {
            head2 :: add(a, tail2)
          } else {
            carry(head1 + head2, add(tail1, tail2))
          }
      }

      override val zero: Nat = Nil

      override def toInt(n: Nat): Int = n.sum

      override def fromInt(i: Int): Nat = {
        require(i >= 0, "Only non-negative integers can be represented")
        BinaryNumber.oneBitRanks(i)
          .map(1 << _)
          .sorted
      }

      override def invariantViolations(n: Nat): List[String] = {
        val monotonicallyIncreasing = n.zipWithIndex.sliding(2).toList.collect {
          case (weight1, offset) :: (weight2, _) :: _ if weight1 >= weight2 =>
            s"monotonically increasing order is required, but $weight1 comes before $weight2 at offset $offset"
        }
        val onlyPowersOfTwo = n.zipWithIndex.collect {
          case (weight, offset) if BinaryNumber.oneBitRanks(weight).size != 1 =>
            s"weight $weight at offset $offset is not a power of two"
        }
        val onlyPositiveWeights = n.zipWithIndex.collect {
          case (weight, offset) if weight <= 0 =>
            s"weight $weight at offset $offset is non-positive"
        }
        monotonicallyIncreasing ++ onlyPowersOfTwo ++ onlyPositiveWeights
      }
    }
  }
}