package com.kentfrazier.purefunc.numeric

class SparseBinaryNumberTests
  extends BinaryNumberTests[SparseBinaryNumber.Nat]("SparseBinaryNumber")(SparseBinaryNumber.Nat.SparseBinaryNumberBN, implicitly)
