package com.kentfrazier.purefunc.numeric

/**
 * Redundant segmented binary numbers, with digits in increasing order of significance
 *
 * This representation uses redundancy to allow a degree of laziness in carrying, implementing a concept
 * called recursive slowdown.
 *
 * The invariants required to ensure efficient operations are that the first non-One digit before any Two
 * must be a Zero, and Two cannot be the first digit. Expressed as a regular expression, the digit string would
 * follow the pattern: `^(0*1|0+1*2)*$`
 *
 * See Chapter 9.2.4 for details
 */
object RedundantSegmentedBinaryNumber {

  sealed abstract class Color
  case object Green extends Color
  case object Yellow extends Color
  case object Red extends Color

  sealed abstract class Digits(val color: Color)
  case object Zero extends Digits(Green)
  final case class Ones(count: Int) extends Digits(Yellow)
  case object Two extends Digits(Red)

  type Nat = List[Digits]
  object Nat {
    implicit case object RedundantSegmentedBinaryNumberBN extends BinaryNumber[Nat] {

      private[this] def prependOnes(count: Int, digits: Nat): Nat = {
        if (count == 0) {
          digits
        } else {
          digits match {
            case Ones(oneCount) :: tail =>
              Ones(count + oneCount) :: tail
            case tail =>
              Ones(count) :: tail
          }
        }
      }

      private[this] def prependZeros(count: Int, digits: Nat): Nat = {
        if (count == 0) {
          digits
        } else {
          digits match {
            case Nil =>
              Nil
            case tail =>
              val zeros = List.fill(count)(Zero)
              zeros ++ tail
          }
        }
      }

      private[this] def simpleIncrement(n: Nat): List[Digits] = n match {
        case Nil =>
          Ones(1) :: Nil
        case Zero :: tail =>
          prependOnes(1, tail)
        case Ones(count) :: tail =>
          Two :: prependOnes(count - 1, tail)
        case Two :: _ =>
          throw new IllegalStateException("invariant violation: cannot start with Two")
      }

      /**
       * Called "fixup" in the text, this restores the invariants around where two is allowed
       */
      private[this] def restoreInvariants(digits: List[Digits]): Nat = digits match {
        case Two :: tail =>
          Zero :: simpleIncrement(tail)
        case (ones @ Ones(_)) :: Two :: tail =>
          ones :: Zero :: simpleIncrement(tail)
        case other =>
          other
      }

      override def increment(n: Nat): Nat = restoreInvariants(simpleIncrement(n))

      override def decrement(n: Nat): Nat = n match {
        case Nil =>
          throw BinaryNumber.IllegalSubtractionError
        case Ones(1) :: Nil =>
          Nil
        case Ones(oneCount) :: tail =>
          Zero :: prependOnes(oneCount - 1, tail)
        case Two :: tail =>
          prependOnes(1, tail)
        case Zero :: tail =>
          // this is still O(log2(n)) because of cascading borrows in this case
          prependOnes(1, decrement(tail))
      }

      override def add(n1: Nat, n2: Nat): Nat = (n1, n2) match {
        case (other, Nil) =>
          other
        case (Nil, other) =>
          other
        case (other, invalid @ Two :: _) =>
          add(restoreInvariants(invalid), other)
        case (invalid @ Two :: _, other) =>
          add(restoreInvariants(invalid), other)
        case (other, invalid @ Ones(_) :: Two :: _) =>
          add(other, restoreInvariants(invalid))
        case (invalid @ Ones(_) :: Two :: _, other) =>
          add(other, restoreInvariants(invalid))
        case (Ones(_) :: _, Zero :: _) =>
          add(n2, n1) // handle mixed cases below by flipping order here so we don't have to repeat it all twice
        case (Zero :: tail1, Ones(oneCount) :: tail2) =>
          if (oneCount == 1) {
            restoreInvariants(prependOnes(1, add(tail1, tail2)))
          } else {
            restoreInvariants(prependOnes(1, add(tail1, Ones(oneCount - 1) :: tail2)))
          }
        case (Zero :: tail1, Zero :: tail2) =>
          Zero :: add(tail1, tail2)
        case (Ones(count1) :: tail1, Ones(count2) :: tail2) =>
          if (count1 > count2) {
            Zero :: prependOnes(count2 - 1, simpleIncrement(add(tail2, Ones(count1 - count2) :: tail1)))
          } else if (count1 < count2) {
            Zero :: prependOnes(count1 - 1, simpleIncrement(add(tail1, Ones(count2 - count1) :: tail2)))
          } else { // counts equal
            Zero :: prependOnes(count2 - 1, simpleIncrement(add(tail1, tail2)))
          }
      }

      override val zero: Nat = Nil

      override def fromInt(i: Int): Nat = {
        val oneBits = BinaryNumber.oneBitRanks(i)
        val (firstOneRank, n) = oneBits
          .foldRight[(Option[Int], Nat)]((None, Nil)) {
            case (nextOneRank, (None, blocks)) =>
              (Some(nextOneRank), prependOnes(1, blocks))
            case (nextOneRank, (Some(currentRank), blocks)) =>
              (Some(nextOneRank), prependOnes(1, prependZeros(currentRank - nextOneRank - 1, blocks)))
          }
        prependZeros(firstOneRank.getOrElse(0), n)
      }

      override def toInt(n: Nat): Int = {
        val (finalSum, _) = n.foldLeft((0, 0)) {
          case ((sum, offset), Zero) =>
            (sum, offset + 1)
          case ((sum, offset), Two) =>
            (sum + (2 << offset), offset + 1)
          case ((sum, offset), Ones(count)) =>
            val oneBlockValue = ((1 << count) - 1) << offset
            (sum + oneBlockValue, offset + count)
        }
        finalSum
      }

      override def invariantViolations(n: Nat): List[String] = {
        val endsWithOnesOrTwo = n.lastOption.toList.collect {
          case Zero =>
            "most significant digit must not be Zero"
        }
        val countsStrictlyPositive = n.zipWithIndex.collect {
          case (ones @ Ones(count), offset) if count <= 0 =>
            s"invalid non-positive Ones count for element $ones at offset $offset"
        }
        val (greenYellowRedOrdering, _) = n
          .map(n => n -> n.color)
          .zipWithIndex.foldLeft[(List[String], Boolean)](Nil, false) {
          case ((violations, _), ((_, Green), _)) =>
            (violations, true)
          case ((violations, greenEncountered), ((_, Yellow), _)) =>
            (violations, greenEncountered)
          case ((violations, true), ((_, Red), _)) =>
            (violations, false)
          case ((violations, false), ((element, Red), index)) =>
            val violation = s"invalid Red element $element encountered at index $index without previous Green element"
            (violation :: violations, false)
        }
        val noContiguousOneBlocks = n.zipWithIndex.sliding(2).toList.collect {
          case (o1 @ Ones(_), offset) :: (o2 @ Ones(_), _) :: _ =>
            s"illegal contiguous Ones blocks $o1 and $o2 starting at offset $offset"
        }
        endsWithOnesOrTwo ++ countsStrictlyPositive ++ greenYellowRedOrdering ++ noContiguousOneBlocks
      }

    }
  }

}
