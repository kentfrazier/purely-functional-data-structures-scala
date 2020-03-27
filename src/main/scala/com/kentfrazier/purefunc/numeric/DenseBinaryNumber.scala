package com.kentfrazier.purefunc.numeric

/**
 * Dense representation of binary numbers, with digits in increasing order of significance
 *
 * See Chapter 9.2
 */
object DenseBinaryNumber {
  sealed abstract class Digit
  case object Zero extends Digit
  case object One extends Digit

  type Nat = List[Digit]
  object Nat {
    implicit case object DenseBinaryNumberBN extends BinaryNumber[Nat] {

      override def increment(n: Nat): Nat = n match {
        case Nil =>
          One :: Nil
        case Zero :: rest =>
          One :: rest
        case One :: rest =>
          Zero :: increment(rest) // carry
      }

      override def decrement(n: Nat): Nat = n match {
        case Nil =>
          throw BinaryNumber.IllegalSubtractionError
        case One :: Nil =>
          Nil
        case One :: rest =>
          Zero :: rest
        case Zero :: rest =>
          One :: decrement(rest) // borrow
      }

      override def add(n1: Nat, n2: Nat): Nat = (n1, n2) match {
        case (Nil, other) =>
          other
        case (other, Nil) =>
          other
        case (head1 :: rest1, head2 :: rest2) =>
          (head1, head2) match {
            case (Zero, Zero) =>
              Zero :: add(rest1, rest2)
            case (One, Zero) | (Zero, One) =>
              One :: add(rest1, rest2)
            case (One, One) =>
              Zero :: increment(add(rest1, rest2))
          }
      }

      override val zero: Nat = Nil

      override def toInt(n: Nat): Int = {
        n.zipWithIndex
          .collect {
            case (One, rank) =>
              1 << rank
          }
          .sum
      }

      override def fromInt(i: Int): Nat = {
        require(i >= 0, "Only non-negative integers can be represented")
        val oneBits = BinaryNumber.oneBitRanks(i).toSet
        (0 to oneBits.max)
          .map { rank =>
            if (oneBits(rank)) {
              One
            } else {
              Zero
            }
          }
          .toList
      }

      override def invariantViolations(n: Nat): List[String] = n.lastOption.toList.filter(_ == Zero).map { _ =>
        "most significant bit must not be zero"
      }

    }
  }
}