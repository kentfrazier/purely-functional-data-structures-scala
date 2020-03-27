package com.kentfrazier.purefunc.numeric

class RedundantBinaryNumberTests
  extends BinaryNumberTests[RedundantBinaryNumber.Nat]("RedundantBinaryNumber")(RedundantBinaryNumber.Nat.RedundantBinaryNumberBN, implicitly)
