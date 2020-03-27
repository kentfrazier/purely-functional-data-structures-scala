package com.kentfrazier.purefunc.numeric

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

abstract class BinaryNumberTests[N](
  val name: String
)(
  implicit
  val bn: BinaryNumber[N]
) extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  val numGen: Gen[N] = Gen.choose(0, Int.MaxValue).map(bn.fromInt)


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
      forAll(Gen.choose(1, Int.MaxValue).map(bn.fromInt)) { n =>
        bn.decrement(bn.increment(n)) shouldEqual n
        bn.increment(bn.decrement(n)) shouldEqual n
      }
    }
  }

}
