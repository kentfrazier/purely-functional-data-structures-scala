package com.kentfrazier.purefunc.numeric

import org.scalacheck.Gen

class SegmentedBinaryNumberTests
  extends BinaryNumberTests[SegmentedBinaryNumber.Nat]("SegmentedBinaryNumber")(SegmentedBinaryNumber.Nat.SegmentedBinaryNumberBN, implicitly) {

  import SegmentedBinaryNumber._

  val knownValues: Seq[(Int, Nat)] = Seq(
    (0, Nil),
    (1, List(Ones(1))),
    (2, List(Zeros(1), Ones(1))),
    (7, List(Ones(3))),
    (22, List(Zeros(1), Ones(2), Zeros(1), Ones(1))),
    (32, List(Zeros(5), Ones(1))),
    (123875, List(Ones(2), Zeros(3), Ones(5), Zeros(3), Ones(4))),
    (89297634, List(Zeros(1), Ones(1), Zeros(3), Ones(3), Zeros(1), Ones(1), Zeros(2), Ones(1), Zeros(2), Ones(1), Zeros(1), Ones(1), Zeros(2), Ones(1), Zeros(1), Ones(1), Zeros(1), Ones(1), Zeros(1), Ones(1))),
    (2019006550, List(Zeros(1), Ones(2), Zeros(1), Ones(1), Zeros(1), Ones(1), Zeros(4), Ones(2), Zeros(2), Ones(4), Zeros(1), Ones(1), Zeros(1), Ones(1), Zeros(4), Ones(4))),
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