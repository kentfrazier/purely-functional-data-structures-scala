package com.kentfrazier.purefunc.numeric

import scala.util.{Failure, Success, Try}

/**
 * Dense representation of binary numbers, with digits in increasing order of significance
 *
 * See Chapter 9.2
 */
object DenseBinaryNumber {
  sealed trait Digit
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
          throw BinaryNumber.IllegalDecrementError
        case One :: Nil =>
          Nil
        case One :: rest =>
          Zero :: rest
        case Zero :: rest =>
          One :: decrement(rest) // borrow

      }
    }
  }
}