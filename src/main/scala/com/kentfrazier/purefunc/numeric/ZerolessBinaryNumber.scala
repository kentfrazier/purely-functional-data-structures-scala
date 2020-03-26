package com.kentfrazier.purefunc.numeric

/**
 * Dense representation of zeroless binary numbers, with digits in increasing order of significance
 *
 * See Chapter 9.2
 */
object ZerolessBinaryNumber {

  sealed abstract class Digit
  case object One extends Digit
  case object Two extends Digit

  type Nat = List[Digit]
  object Nat {
    implicit case object DenseBinaryNumberBN extends BinaryNumber[Nat] {

      override def increment(n: Nat): Nat = n match {
        case Nil =>
          One :: Nil
        case One :: tail =>
          Two :: tail
        case Two :: tail =>
          One :: increment(tail) // carry
      }

      override def decrement(n: Nat): Nat = n match {
        case Nil =>
          throw BinaryNumber.IllegalSubtractionError
        case One :: Nil =>
          Nil
        case Two :: tail =>
          One :: tail
        case One :: tail =>
          Two :: decrement(tail) // borrow
      }

      override def add(n1: Nat, n2: Nat): Nat = (n1, n2) match {
        case (Nil, other) =>
          other
        case (other, Nil) =>
          other
        case (head1 :: tail1, head2 :: tail2) =>
          (head1, head2) match {
            case (One, One) =>
              Two :: add(tail1, tail2)
            case (One, Two) | (Two, One) =>
              One :: increment(add(tail1, tail2))
            case (Two, Two) =>
              Two :: increment(add(tail1, tail2))
          }
      }
      override val zero: Nat = Nil

      override def toInt(n: Nat): Int = {
        n.zipWithIndex
          .map {
            case (One, rank) =>
              1 << rank
            case (Two, rank) =>
              2 << rank
          }
          .sum
      }

    }
  }

}
