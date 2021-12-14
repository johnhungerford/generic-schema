package org.hungerford.generic.schema.validator

import org.hungerford.generic.schema.validator.Validator.minExclusive
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

  behavior of "StringLength"

  it should "validate based on minimum length inclusively" in {
    val validator = Validator.minLength( 5 )
    validator.isValid( "1234" ) shouldBe false
    validator.isValid( "12345" ) shouldBe true
    validator.isValid( "123456" ) shouldBe true
  }

  it should "validate based on minimum length exclusively" in {
    val lenVal = Validator.minExclusive[ Int ]( 4 )
    "1234".length shouldBe 4
    lenVal.isValid( "1234".length ) shouldBe false

    val validator = Validator.minLengthExclusive( 5 )
    validator.isValid( "1234" ) shouldBe false
    validator.isValid( "12345" ) shouldBe false
    validator.isValid( "123456" ) shouldBe true
  }

  it should "validate based on maximum length inclusively" in {
    val validator = Validator.maxLength( 5 )
    validator.isValid( "1234" ) shouldBe true
    validator.isValid( "12345" ) shouldBe true
    validator.isValid( "123456" ) shouldBe false
  }

  it should "validate based on maximum length exclusively" in {
    val validator = Validator.maxLengthExclusive( 5 )
    validator.isValid( "1234" ) shouldBe true
    validator.isValid( "12345" ) shouldBe false
    validator.isValid( "123456" ) shouldBe false
  }

  it should "validate based on exact length" in {
    val validator = Validator.length( 5 )
    validator.isValid( "1234" ) shouldBe false
    validator.isValid( "12345" ) shouldBe true
    validator.isValid( "123456" ) shouldBe false
  }

}
