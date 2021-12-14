package org.hungerford.generic.schema.validator

import org.scalatest.flatspec.AnyFlatSpecLike

class StringValidatorTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

  behavior of "Regx"

  it should "validate a string using a regex instance" in {
    val validator = Validator.regex( """[0-9a-z]+""".r )
    validator.isValid( "" ) shouldBe false
    validator.isValid( "a" ) shouldBe true
    validator.isValid( "A" ) shouldBe false
    validator.isValid( "234a3x2c42" ) shouldBe true
    validator.isValid( "234a3-x2c42" ) shouldBe false
  }

  it should "validate a string using a regex string" in {
    val validator = Validator.regex( """[0-9a-z]+""" )
    validator.isValid( "" ) shouldBe false
    validator.isValid( "a" ) shouldBe true
    validator.isValid( "A" ) shouldBe false
    validator.isValid( "234a3x2c42" ) shouldBe true
    validator.isValid( "234a3-x2c42" ) shouldBe false
  }


}
