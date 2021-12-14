package org.hungerford.generic.schema.validator

import org.scalatest.flatspec.AnyFlatSpecLike

class NumericValidatorsTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

  behavior of "PositiveOrZero"

  it should "validate positive numbers and zero, and invalidate negative numbers" in {
    def validator[ T : Numeric ] = Validator.positiveOrZero[ T ]
    validator.isValid( 5 ) shouldBe true
    validator.isValid( 0.0000002123 ) shouldBe true
    validator.isValid( BigInt( "3425234524523452352354" ) ) shouldBe true

    validator.isValid( 0 ) shouldBe true
    validator.isValid( 0.00 ) shouldBe true
    validator.isValid( BigInt( "0" ) ) shouldBe true

    validator.isValid( -1 ) shouldBe false
    validator.isValid( -0.0000000000001 ) shouldBe false
    validator.isValid( BigInt( "-99999999999999999999999999999" ) ) shouldBe false
    validator.isValid( -34235234 ) shouldBe false
  }

  behavior of "NegativeOrZero"

  it should "validate negative numbers and zero, and invalidate positive numbers" in {
    def validator[ T : Numeric ] = Validator.negativeOrZero[ T ]
    validator.isValid( 5 ) shouldBe false
    validator.isValid( 0.0000002123 ) shouldBe false
    validator.isValid( BigInt( "3425234524523452352354" ) ) shouldBe false

    validator.isValid( 0 ) shouldBe true
    validator.isValid( 0.00 ) shouldBe true
    validator.isValid( BigInt( "0" ) ) shouldBe true

    validator.isValid( -1 ) shouldBe true
    validator.isValid( -0.0000000000001 ) shouldBe true
    validator.isValid( BigInt( "-99999999999999999999999999999" ) ) shouldBe true
    validator.isValid( -34235234 ) shouldBe true
  }

  behavior of "NonZero"

  it should "validate all numbers besides zero" in {
    def validator[ T : Numeric ] = Validator.nonZero[ T ]
    validator.isValid( 5 ) shouldBe true
    validator.isValid( 0.0000002123 ) shouldBe true
    validator.isValid( BigInt( "3425234524523452352354" ) ) shouldBe true

    validator.isValid( 0 ) shouldBe false
    validator.isValid( 0.00 ) shouldBe false
    validator.isValid( BigInt( "0" ) ) shouldBe false

    validator.isValid( -1 ) shouldBe true
    validator.isValid( -0.0000000000001 ) shouldBe true
    validator.isValid( BigInt( "-99999999999999999999999999999" ) ) shouldBe true
    validator.isValid( -34235234 ) shouldBe true
  }
}
