package kse.unit4.challenge

import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}
import org.scalacheck.Test.Parameters

object NumeralsSpecification extends Properties("Numerals"):

  override def overrideParameters(p: Parameters): Parameters =
    p.withMinSuccessfulTests(50).withMaxDiscardRatio(100)

  include(isZeroSpecification)
  include(SuccessorSpecification)
  include(PredecessorSpecification)
  include(AdditionSpecification)
  include(SubtractionSpecification)
  include(CombinedOperationsSpecification)
  include(ComparisonSpecification)

end NumeralsSpecification

object isZeroSpecification extends Properties("isZero"):

  property("Zero is zero") = propBoolean:
    Zero.isZero

  property("Successor is not zero") = forAll: (n: Numeral) =>
    !Successor(n).isZero

end isZeroSpecification

object PredecessorSpecification extends Properties("Predecessor"):

  property("Zero predecessor is Zero") = propBoolean:
    Zero.predecessor == Zero

  property("Successor predecessor is the original numeral") = forAll: (n: Numeral) =>
    Successor(n).predecessor == n

  property("n predecessor is (n - 1) for n != 0") = forAll: (n: Numeral) =>
    !n.isZero ==> (n.predecessor.toInt == (n.toInt - 1))

end PredecessorSpecification

object SuccessorSpecification extends Properties("Successor"):

  property("Zero successor is Successor(Zero)") = propBoolean:
    Zero.successor == Successor(Zero)

  property("Successor successor is Successor(Successor)") = forAll: (n: Numeral) =>
    Successor(n).successor == Successor(Successor(n))

  property("n successor is (n + 1)") = forAll: (n: Numeral) =>
    n.successor.toInt == (n.toInt + 1)

end SuccessorSpecification

object AdditionSpecification extends Properties("Addition"):

  property("Zero + Zero == Zero") = propBoolean:
    (Zero + Zero) == Zero

  property("Zero + n == n") = forAll: (n: Numeral) =>
    (Zero + n) == n

  property("Successor(n) + Zero == Successor(n)") = forAll: (n: Numeral) =>
    (Successor(n) + Zero) == Successor(n)

  property("Successor(n) + Successor(m) == Successor(Successor(n + m))") = forAll: (n: Numeral, m: Numeral) =>
    (Successor(n) + Successor(m)) == Successor(Successor(n + m))

  property("Addition is commutative") = forAll: (a: Numeral, b: Numeral) =>
    (a + b).toInt == (b + a).toInt

  property("Addition is associative") = forAll: (a: Numeral, b: Numeral, c: Numeral) =>
    ((a + b) + c).toInt == (a + (b + c)).toInt

end AdditionSpecification

object SubtractionSpecification extends Properties("Subtraction"):

  property("Zero - Zero == Zero") = propBoolean:
    (Zero - Zero) == Zero

  property("Zero - n is Zero") = forAll: (n: Numeral) =>
    (Zero - n) == Zero

  property("Successor(n) - Zero == Successor(n)") = forAll: (n: Numeral) =>
    (Successor(n) - Zero) == Successor(n)

  property("Successor(n) - Successor(m) == n - m") = forAll: (n: Numeral, m: Numeral) =>
    (Successor(n) - Successor(m)) == n - m

  property("Subtracting larger or equal from value is Zero") = forAll: (a: Numeral, b: Numeral) =>
    (a - (a + b)) == Zero

end SubtractionSpecification

object CombinedOperationsSpecification extends Properties("CombinedOperations") {

  property("Adding and then subtracting the same value should return the original value") = forAll: (a: Numeral, b: Numeral) =>
    ((a + b) - b) == a

  property("Subtraction and then addition should return the original value if original value >= subtrahend") = forAll: (a: Numeral, b: Numeral) =>
    (((a + b) - b) + b) == (a + b)

  property("Subtraction and addition are consistent if original value >= subtrahend") = forAll: (a: Numeral, b: Numeral, c: Numeral, d: Numeral) =>
    (((a + b) - b) + ((c + d) - d)) == ((a + b) + (c + d)) - (b + d)
}

object ComparisonSpecification extends Properties("Comparison"):

  property("Zero == Zero") = propBoolean:
    Zero == Zero

  property("Zero != nonZeroValue") = forAll: (nonZeroValue: Numeral) =>
    !nonZeroValue.isZero ==> (Zero != nonZeroValue)

  property("Zero > Zero") = propBoolean:
    !(Zero > Zero)

  property("Zero >= Zero") = propBoolean:
    Zero >= Zero

  property("Zero < Zero") = propBoolean:
    !(Zero < Zero)

  property("Zero <= Zero") = propBoolean:
    Zero <= Zero

  property("Zero > nonZeroValue") = forAll: (nonZeroValue: Numeral) =>
    !nonZeroValue.isZero ==> !(Zero > nonZeroValue)

  property("Zero >= nonZeroValue") = forAll: (nonZeroValue: Numeral) =>
    !nonZeroValue.isZero ==> !(Zero >= nonZeroValue)

  property("Zero < nonZeroValue") = forAll: (nonZeroValue: Numeral) =>
    !nonZeroValue.isZero ==> Zero < nonZeroValue

  property("Zero <= nonZeroValue") = forAll: (nonZeroValue: Numeral) =>
    !nonZeroValue.isZero ==> Zero <= nonZeroValue

  property("nonZeroValue > Zero") = forAll: (nonZeroValue: Numeral) =>
    !nonZeroValue.isZero ==> nonZeroValue > Zero

  property("successor(n) == successor(n)") = forAll: (n: Numeral) =>
    Successor(n) == Successor(n)

  property("successor(n) != successor(m) if n != m") = forAll: (n: Numeral, m: Numeral) =>
    (n != m) ==> (Successor(n) != Successor(m))

  property("successor(n) > Zero") = forAll: (n: Numeral) =>
    Successor(n) > Zero

  property("successor(n) >= Zero") = forAll: (n: Numeral) =>
    Successor(n) >= Zero

  property("Zero < successor(n)") = forAll: (n: Numeral) =>
    Zero < Successor(n)

  property("Zero <= successor(n)") = forAll: (n: Numeral) =>
    Zero <= Successor(n)

  property("successor(n) > successor(m) if n > m") = forAll: (n: Numeral, m: Numeral) =>
    (n > m) ==> (Successor(n) > Successor(m))

  property("successor(n) >= successor(m) if n > m") = forAll: (n: Numeral, m: Numeral) =>
    (n > m) ==> (Successor(n) >= Successor(m))

  property("successor(n) < successor(m) if n < m") = forAll: (n: Numeral, m: Numeral) =>
    (n < m) ==> (Successor(n) < Successor(m))

  property("successor(n) <= successor(m) if n < m") = forAll: (n: Numeral, m: Numeral) =>
    (n < m) ==> (Successor(n) <= Successor(m))

end ComparisonSpecification
