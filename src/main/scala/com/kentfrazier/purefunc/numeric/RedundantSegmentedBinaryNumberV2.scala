package com.kentfrazier.purefunc.numeric

/**
 * Redundant segmented binary numbers, with digits in increasing order of significance
 *
 * This is the solution for Exercise 9.12
 */
object RedundantSegmentedBinaryNumberV2 {

  sealed abstract class Color
  case object Green extends Color
  case object Yellow extends Color
  case object Red extends Color

  sealed abstract class Digits(val color: Color)
  case object Zero extends Digits(Red)
  final case class Ones(count: Int) extends Digits(Yellow)
  final case class Twos(count: Int) extends Digits(Green)
  final case class Threes(count: Int) extends Digits(Yellow)
  case object Four extends Digits(Red)

  type Nat = List[Digits]
  object Nat {
    implicit case object RedundantSegmentedBinaryNumberV2BN extends BinaryNumber[Nat] {

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

      private[this] def prependTwos(count: Int, digits: Nat): Nat = {
        if (count == 0) {
          digits
        } else {
          digits match {
            case Twos(twoCount) :: tail =>
              Twos(count + twoCount) :: tail
            case tail =>
              Twos(count) :: tail
          }
        }
      }

      private[this] def prependThrees(count: Int, digits: Nat): Nat = {
        if (count == 0) {
          digits
        } else {
          digits match {
            case Threes(threesCount) :: tail =>
              Threes(count + threesCount) :: tail
            case tail =>
              Threes(count) :: tail
          }
        }
      }

      private[this] def simpleIncrement(n: Nat): List[Digits] = n match {
        case Nil =>
          Ones(1) :: Nil
        case Zero :: tail =>
          prependOnes(1, tail)
        case Ones(count) :: tail =>
          prependTwos(1, prependOnes(count - 1, tail))
        case Twos(count) :: tail =>
          prependThrees(1, prependTwos(count - 1, tail))
        case Threes(count) :: tail =>
          Four :: prependThrees(count - 1, tail)
        case Four :: _ =>
          throw new IllegalStateException("invariant violation: cannot start with Four")
      }

      private[this] def simpleDecrement(n: Nat): List[Digits] = n match {
        case Nil =>
          throw BinaryNumber.IllegalSubtractionError
        case Four :: tail =>
          prependThrees(1, tail)
        case Threes(count) :: tail =>
          prependTwos(1, prependThrees(count - 1, tail))
        case Twos(count) :: tail =>
          prependOnes(1, prependTwos(count - 1, tail))
        case Ones(1) :: Nil =>
          Nil
        case Ones(count) :: tail =>
          Zero :: prependOnes(count - 1, tail)
        case Zero :: _ =>
          throw new IllegalStateException("invariant violation: cannot start with Zero")
      }

      /**
       * Called "fixup" in the text, this restores the invariants around where two is allowed
       */
      private[this] def restoreInvariants(digits: List[Digits]): Nat = digits match {
        case Zero :: tail =>
          prependTwos(1, simpleDecrement(tail))
        case Ones(count) :: Zero :: tail =>
          Ones(count) :: prependTwos(1, simpleDecrement(tail))
        case Threes(count) :: Zero :: tail =>
          Threes(count) :: prependTwos(1, simpleDecrement(tail))
        case Four :: tail =>
          prependTwos(1, simpleIncrement(tail))
        case Ones(count) :: Four :: tail =>
          Ones(count) :: prependTwos(1, simpleIncrement(tail))
        case Threes(count) :: Four :: tail =>
          Threes(count) :: prependTwos(1, simpleIncrement(tail))
        case other =>
          other
      }

      override def increment(n: Nat): Nat = restoreInvariants(simpleIncrement(n))

      override def decrement(n: Nat): Nat = restoreInvariants(simpleDecrement(n))

      override def add(n1: Nat, n2: Nat): Nat = (restoreInvariants(n1), restoreInvariants(n2)) match {
        case (other, Nil) =>
          other
        case (Nil, other) =>
          other
        case (Threes(_) :: _, Twos(_) :: _) | (Threes(_) :: _, Ones(_) :: _) | (Twos(_) :: _, Ones(_) :: _) =>
          add(n2, n1) // handle descending cases by flipping order here so we don't have to repeat it all twice
        case (Ones(count1) :: tail1, Ones(count2) :: tail2) =>
          prependTwos(
            math.min(count1, count2),
            add(
              prependOnes(math.max(count1 - count2, 0), tail1),
              prependOnes(math.max(count2 - count1, 0), tail2),
            )
          )
        case (Ones(oneCount) :: tail1, Twos(twoCount) :: tail2) =>
          restoreInvariants(
            prependThrees(
              math.min(oneCount, twoCount),
              add(
                prependOnes(math.max(oneCount - twoCount, 0), tail1),
                prependTwos(math.max(twoCount - oneCount, 0), tail2)
              )
            )
          )
        case (Ones(oneCount) :: tail1, Threes(threeCount) :: tail2) =>
          prependTwos(1, prependThrees(
            math.min(oneCount, threeCount) - 1,
            increment(add(
              prependOnes(math.max(oneCount - threeCount, 0), tail1),
              prependThrees(math.max(threeCount - oneCount, 0), tail2)
            ))
          ))
        case (Twos(count1) :: tail1, Twos(count2) :: tail2) =>
          prependTwos(1, prependThrees(
            math.min(count1, count2) - 1,
            increment(add(
              prependTwos(math.max(count1 - count2, 0), tail1),
              prependTwos(math.max(count2 - count1, 0), tail2)
            ))
          ))
        case (Twos(twoCount) :: tail1, Threes(threeCount) :: tail2) =>
          restoreInvariants(
            prependOnes(
              math.min(twoCount, threeCount),
              increment(increment(add(
                prependTwos(math.max(twoCount - threeCount, 0), tail1),
                prependThrees(math.max(threeCount - twoCount, 0), tail2)
              )))
            )
          )
        case (Threes(count1) :: tail1, Threes(count2) :: tail2) =>
          val count = math.min(count1, count2)
          val tailSum = add(
            prependThrees(math.max(count1 - count2, 0), tail1),
            prependThrees(math.max(count2 - count1, 0), tail2)
          )
          if (count == 1) {
            prependTwos(1, increment(increment(tailSum)))
          } else if (count == 2) {
            prependTwos(1, Four :: increment(increment(tailSum)))
          } else {
            prependTwos(2, prependThrees(
              count - 2,
              increment(increment(increment(tailSum)))
            ))
          }
        case other =>
          throw new IllegalStateException(s"Invariant violated, Red element at head of list: $other")
      }

      override val zero: Nat = Nil

      override def fromInt(i: Int): Nat = {
        def prependZeros(count: Int, n: Nat): Nat = {
          (0 until count).foldLeft(n) {
            case (current, _) =>
              restoreInvariants(Zero :: current)
          }
        }
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
          case ((sum, offset), Ones(count)) =>
            val oneBlockValue = ((1 << count) - 1) << offset
            (sum + oneBlockValue, offset + count)
          case ((sum, offset), Twos(count)) =>
            val oneBlockValue = ((1 << count) - 1) << offset
            (sum + (oneBlockValue << 1), offset + count)
          case ((sum, offset), Threes(count)) =>
            val oneBlockValue = ((1 << count) - 1) << offset
            (sum + oneBlockValue + (oneBlockValue << 1), offset + count)
          case ((sum, offset), Four) =>
            (sum + (4 << offset), offset + 1)
        }
        finalSum
      }

      override def invariantViolations(n: Nat): List[String] = {
        val doesNotEndWithZero = n.lastOption.toList.collect {
          case Zero =>
            "most significant digit must not be Zero"
        }
        val countsStrictlyPositive = n.zipWithIndex.collect {
          case (ones @ Ones(count), offset) if count <= 0 =>
            s"invalid non-positive Ones count for element $ones at offset $offset"
          case (ones @ Twos(count), offset) if count <= 0 =>
            s"invalid non-positive Twos count for element $ones at offset $offset"
          case (ones @ Threes(count), offset) if count <= 0 =>
            s"invalid non-positive Threes count for element $ones at offset $offset"
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
        doesNotEndWithZero ++ countsStrictlyPositive ++ greenYellowRedOrdering ++ noContiguousOneBlocks
      }

    }
  }

}
