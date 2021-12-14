package org.hungerford.generic.schema.validator

import org.scalatest.flatspec.AnyFlatSpecLike

class EnumValidatorTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

  behavior of "OneOf"

  it should "correctly validate a value against a set of allowed values" in {
    val validator = Validator.oneOf( 3, 6, 9 )
    validator.isValid( 0 ) shouldBe false
    validator.isValid( 4 ) shouldBe false
    validator.isValid( 5 ) shouldBe false
    validator.isValid( 3 ) shouldBe true
    validator.isValid( 6 ) shouldBe true
    validator.isValid( 9 ) shouldBe true
  }

  behavior of "NoneOf"

  it should "correctly validate a value against a set of excluded values" in {
    val validator = Validator.noneOf( 3, 6, 9 )
    validator.isValid( 0 ) shouldBe true
    validator.isValid( 4 ) shouldBe true
    validator.isValid( 5 ) shouldBe true
    validator.isValid( 3 ) shouldBe false
    validator.isValid( 6 ) shouldBe false
    validator.isValid( 9 ) shouldBe false
  }

}
