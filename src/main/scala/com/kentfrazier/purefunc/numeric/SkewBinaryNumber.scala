package com.kentfrazier.purefunc.numeric

import scala.annotation.tailrec

/**
 * Sparse Skew Binary Number representation
 *
 * See Chapter 9.3
 */
object SkewBinaryNumber {

  type Nat = List[Int]
  object Nat {
    implicit object SkewBinaryNumberBN extends BinaryNumber[Nat] {

      override def increment(n: Nat): Nat = n match {
        case weight1 :: weight2 :: tail if weight1 == weight2 =>
          // this is the Two case. Due to the invariant that a Two can only occur in the first non-zero position,
          // we can blindly increment the following digit.
          (weight1 + weight2 + 1) :: tail
        case tail =>
          // in any other case, we can just add a One to the front (possibly duplicating rank 0, but that's okay,
          // because if it were Two it would have been handled above, and we can increment Zero or One without
          // issue)
          1 :: tail
      }

      override def decrement(n: Nat): Nat = n match {
        case Nil =>
          throw BinaryNumber.IllegalSubtractionError
        case 1 :: tail =>
          tail
        case head :: tail =>
          val predecessor = head >> 1 // break least significant digit into two of the previous digit
          predecessor :: predecessor :: tail
      }

      private[this] def concat(front: Nat, back: Nat): Nat = (front, back) match {
        case (Nil, finished) =>
          finished
        case (_front @ headF :: _, headB :: tailB) if headF > headB =>
          concat(headB :: Nil, concat(_front, tailB))
        case (_front, _back @ two1 :: two2 :: _) if two1 == two2 =>
          concat(decrement(_front), increment(_back))
        case (_validFront, _validBack) =>
          _validFront ++ _validBack
      }

      override def add(n1: Nat, n2: Nat): Nat = (n1, n2) match {
        case (Nil, other) =>
          other
        case (other, Nil) =>
          other
        case (a, b) =>
          Iterator
            .iterate((a.reverse, b.reverse, zero)) {
              case (Nil, Nil, n) =>
                (Nil, Nil, n)
              case (digit :: tail, Nil, n) =>
                (tail, Nil, concat(digit :: Nil, n))
              case (Nil, digit :: tail, n) =>
                (tail, Nil, concat(digit :: Nil, n))
              case (digits1 @ head1 :: tail1, digits2 @ head2 :: tail2, n) =>
                if (head1 > head2) {
                  (tail1, digits2, concat(head1 :: Nil, n))
                } else {
                  (digits1, tail2, concat(head2 :: Nil, n))
                }
            }
            .dropWhile {
              case (digits1, digits2, _) =>
                digits1.nonEmpty || digits2.nonEmpty
            }
            .next()
            ._3
      }

      override val zero: Nat = Nil

      override def toInt(n: Nat): Int = n.sum

      override def fromInt(i: Int): Nat = {
        Iterator
          .iterate((i, zero)) {
            case (remaining, n) =>
              if (remaining == 0) {
                (remaining, n)
              } else {
                val largestPowerOfTwo = Integer.highestOneBit(remaining + 1)
                val weight = largestPowerOfTwo - 1
                (remaining - weight, weight :: n)
              }
          }
          .dropWhile(_._1 > 0)
          .next()
          ._2
      }

      override def invariantViolations(n: Nat): List[String] = {
        val outOfOrder = n.zipWithIndex.sliding(2).toList.collect {
          case (d1, offset) :: (d2, _) :: _ if d1 > d2 =>
            s"digits $d1 and $d2 out of order at offset $offset"
        }
        val invalidTwo = n.zipWithIndex.drop(1).sliding(2).toList.collect {
          case (d1, offset) :: (d2, _) :: _ if d1 == d2 =>
            s"Two in illegal non-leading position with value $d1 at offset $offset"
        }
        val invalidDigits = n.zipWithIndex
          .filter {
            case (digit, _) =>
              Integer.bitCount(digit + 1) != 1
          }
          .map {
            case (digit, offset) =>
              s"invalid digit weight $digit at offset $offset"
          }
        outOfOrder ++ invalidTwo ++ invalidDigits
      }

    }
  }

}
