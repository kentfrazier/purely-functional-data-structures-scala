package com.kentfrazier.purefunc.numeric

import org.scalacheck.{Gen, Shrink}
import org.scalactic.Equality
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

abstract class BinaryNumberTests[N](
  val name: String
)(
  implicit
  val bn: BinaryNumber[N]
) extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit def shrink[A]: Shrink[A] = Shrink.shrinkAny // disable shrinking

  implicit object BinaryNumberEquals extends Equality[N] {
    override def areEqual(a: N, b: Any): Boolean = b match {
      case n: N =>
        bn.eq(a, n)
      case _ =>
        false
    }
  }

  val numGen: Gen[N] = Gen.choose(0, Int.MaxValue).map(bn.fromInt)

  /**
   * safe generator to prevent integer overflow when adding for up to three addends
   */
  val addendsGen: Gen[(N, N, N)] = for {
    a1 <- Gen.choose(0, Int.MaxValue)
    a2 <- Gen.choose(0, Int.MaxValue - a1)
    a3 <- Gen.choose(0, Int.MaxValue - a1 - a2)
  } yield (bn.fromInt(a1), bn.fromInt(a2), bn.fromInt(a3))

  "fromInt" should {
    "always maintain invariants" in {
      forAll(Gen.choose(0, Int.MaxValue)) { n =>
        bn.invariantViolations(bn.fromInt(n)) should have size 0
      }
    }
  }

  "fromInt and toInt" should {
    "always be symmetrical" in {
      forAll(Gen.choose(0, Int.MaxValue)) { i =>
        val fromInt = bn.fromInt(i)
        val toInt = bn.toInt(fromInt)
        toInt shouldEqual i
      }
    }
  }

  "increment and decrement" should {
    "always be symmetrical" in {
      forAll(Gen.choose(1, Int.MaxValue - 1).map(bn.fromInt)) { n =>
        bn.decrement(bn.increment(n)) shouldEqual n
        bn.increment(bn.decrement(n)) shouldEqual n
      }
    }
  }

  "increment" should {

    "always maintain invariants" in {
      forAll(Gen.choose(0, Int.MaxValue - 1).map(bn.fromInt)) { n =>
        bn.invariantViolations(bn.increment(n)) should have size 0
      }
    }

    "return a value equal to n + 1" in {
      forAll(Gen.choose(0, Int.MaxValue - 1)) { n =>
        bn.increment(bn.fromInt(n)) shouldEqual bn.fromInt(n + 1)
      }
    }

  }

  "increment" when {

    "called on zero" should {
      "return one" in {
        bn.increment(bn.zero) shouldEqual bn.one
      }
    }

    "called n times on zero" should {
      "return n" in {
        forAll(Gen.choose(0, 1000)) { times =>
          val n = Iterator.iterate(bn.zero)(bn.increment).drop(times).next()
          n shouldEqual bn.fromInt(times)
        }
      }
    }

  }

  "decrement" should {

    "always maintain invariants" in {
      forAll(Gen.choose(1, Int.MaxValue).map(bn.fromInt)) { n =>
        bn.invariantViolations(bn.decrement(n)) should have size 0
      }
    }

    "return a value equal to n - 1" in {
      forAll(Gen.choose(0, Int.MaxValue - 1)) { n =>
        bn.decrement(bn.fromInt(n + 1)) shouldEqual bn.fromInt(n)
      }
    }

  }

  "decrement" when {

    "called on one" should {
      "return zero" in {
        bn.decrement(bn.one) shouldEqual bn.zero
      }
    }

    "called on zero" should {
      "throw IllegalSubtractionError" in {
        an [BinaryNumber.IllegalSubtractionError.type] should be thrownBy {
          bn.decrement(bn.zero)
        }
      }
    }

  }

  "add" should {

    "always maintain invariants" in {
      forAll(addendsGen) {
        case (a, b, _) =>
        bn.invariantViolations(bn.add(a, b)) should have size 0
      }
    }

    "have the property of additive identity with zero, whether on the left or right" in {
      forAll(numGen) { n =>
        bn.add(bn.zero, n) shouldEqual n
        bn.add(n, bn.zero) shouldEqual n
      }
    }

    "have the commutative property" in {
      forAll(addendsGen) {
        case (a, b, _) =>
          bn.add(a, b) shouldEqual bn.add(b, a)
      }
    }

    "have the associative property" in {
      forAll(addendsGen) {
        case (a, b, c) =>
          bn.add(a, bn.add(b, c)) shouldEqual bn.add(bn.add(a, b), c)
      }
    }

    "return the same value as the Int-based computation" in {
      forAll(addendsGen) {
        case (a, b, _) =>
          bn.toInt(bn.add(a, b)) shouldEqual (bn.toInt(a) + bn.toInt(b))
      }
    }

  }

}
