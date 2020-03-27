package com.kentfrazier.purefunc.numeric

object SegmentedBinaryNumber {

  sealed abstract class DigitBlock
  final case class Zeros(count: Int) extends DigitBlock
  final case class Ones(count: Int) extends DigitBlock

  type Nat = List[DigitBlock]
  object Nat {
    implicit case object SegmentedBinaryNumberBN extends BinaryNumber[Nat] {

      def prependOnes(count: Int, blocks: Nat): Nat = {
        if (count == 0) {
          blocks
        } else {
          blocks match {
            case Ones(oneCount) :: tail =>
              Ones(count + oneCount) :: tail
            case tail =>
              Ones(count) :: tail
          }
        }
      }

      def prependZeros(count: Int, blocks: Nat): Nat = {
        if (count == 0) {
          blocks
        } else {
          blocks match {
            case Nil =>
              Nil
            case Zeros(zeroCount) :: tail =>
              Zeros(count + zeroCount) :: tail
            case tail =>
              Zeros(count) :: tail
          }
        }
      }

      override def increment(n: Nat): Nat = n match {
        case Nil =>
          Ones(1) :: Nil
        case Zeros(zeroCount) :: tail =>
          prependOnes(1, prependZeros(zeroCount - 1, tail))
        case Ones(count) :: tail =>
          Zeros(count) :: increment(tail) // carry across whole block
      }

      override def decrement(n: Nat): Nat = n match {
        case Nil =>
          throw BinaryNumber.IllegalSubtractionError
        case Ones(oneCount) :: tail =>
          prependZeros(1, prependOnes(oneCount - 1, tail))
        case Zeros(count) :: tail =>
          Ones(count) :: decrement(tail) // borrow across whole block
      }

      override def add(n1: Nat, n2: Nat): Nat = (n1, n2) match {
        case (other, Nil) =>
          other
        case (Nil, other) =>
          other
        case (Ones(_) :: _, Zeros(_) :: _) =>
          add(n2, n1) // handle mixed case below by flipping order here so we don't have to repeat it all twice
        case (Zeros(zeroCount) :: tail1, Ones(oneCount) :: tail2) =>
          if (zeroCount > oneCount) {
            prependOnes(oneCount, prependZeros(zeroCount - oneCount, add(tail2, tail1)))
          } else if (zeroCount < oneCount) {
            prependOnes(zeroCount, add(tail1, Ones(oneCount - zeroCount) :: tail2))
          } else { // counts equal
            prependOnes(oneCount, add(tail1, tail2))
          }
        case (Zeros(count1) :: tail1, Zeros(count2) :: tail2) =>
          if (count1 > count2) {
            prependZeros(count2, add(Zeros(count1 - count2) :: tail1, tail2))
          } else if (count1 < count2) {
            prependZeros(count1, add(Zeros(count2 - count1) :: tail2, tail1))
          } else { // counts equal
            prependZeros(count2, add(tail1, tail2))
          }
        case (Ones(count1) :: tail1, Ones(count2) :: tail2) =>
          if (count1 > count2) {
            prependZeros(count2, increment(add(tail2, Ones(count1 - count2) :: tail1)))
          } else if (count1 < count2) {
            prependZeros(count1, increment(add(tail1, Ones(count2 - count1) :: tail2)))
          } else { // counts equal
            prependZeros(count2, increment(add(tail1, tail2)))
          }
      }

      override val zero: Nat = Nil

      override def toInt(n: Nat): Int = {
        val (finalSum, _) = n.foldLeft((0, 0)) {
          case ((sum, offset), Zeros(count)) =>
            (sum, offset + count)
          case ((sum, offset), Ones(count)) =>
            val oneBlockValue = ((1 << count) - 1) << offset
            (sum + oneBlockValue, offset + count)
        }
        finalSum
      }

      override def invariantViolations(n: Nat): List[String] = {
        val endsWithOnes = n.lastOption.toList.collect {
          case Zeros(_) =>
            "most significant block must not be Zeros"
        }
        val alwaysAlternates = n.zipWithIndex.sliding(2).collect {
          case (o1 @ Ones(_), offset) :: (o2 @ Ones(_), _) :: _ =>
            s"consecutive Ones blocks ($o1, $o2) at offset $offset not allowed"
          case (z1 @ Zeros(_), offset) :: (z2 @ Zeros(_), _) :: _ =>
            s"consecutive Zeros blocks ($z1, $z2) at offset $offset not allowed"
        }
        val countsStrictlyPositive = n.zipWithIndex.collect {
          case (ones @ Ones(count), offset) if count <= 0 =>
            s"invalid non-positive Ones count for element $ones at offset $offset"
          case (zeros @ Zeros(count), offset) if count <= 0 =>
            s"invalid non-positive Zeros count for element $zeros at offset $offset"
        }
        endsWithOnes ++ alwaysAlternates ++ countsStrictlyPositive
      }

    }
  }

}
