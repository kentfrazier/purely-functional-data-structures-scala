package com.kentfrazier.purefunc.numeric

import org.scalacheck.Gen

class RedundantSegmentedBinaryNumberV2Tests
  extends BinaryNumberTests[RedundantSegmentedBinaryNumberV2.Nat]("RedundantSegmentedBinaryNumberV2")(RedundantSegmentedBinaryNumberV2.Nat.RedundantSegmentedBinaryNumberV2BN, implicitly) {

  import RedundantSegmentedBinaryNumberV2._

  val knownValues: Seq[(Int, Nat)] = Seq(
    (0, Nil),
    (1, List(Ones(1))),
    (2, List(Twos(1))),
    (7, List(Ones(3))),
    (22, List(Twos(1), Zero, Ones(1), Twos(1))),
    (32, List(Twos(1), Ones(4))),
  )

  "toInt" should {
    "return expected results for known values" in {
      knownValues.foreach {
        case (int, nat) =>
          bn.toInt(nat) shouldEqual int
      }
    }
  }

  "fromInt" should {
    "return expected results for known values" in {
      knownValues.foreach {
        case (int, nat) =>
          bn.fromInt(int) shouldEqual nat
      }
    }
  }

  "add" should {
    "return expected sum for known values" in {
      forAll(Gen.listOfN(2, Gen.oneOf(knownValues))) {
        case List((int1, nat1), (int2, nat2)) =>
          bn.add(nat1, nat2) shouldEqual bn.fromInt(int1 + int2)
        case _ =>
          fail() // shouldn't hit this branch
      }
    }
  }

}
