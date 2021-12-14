package org.hungerford.generic.schema.tapir

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.validator.Validator
import sttp.tapir.{Validator as TapirValidator, ValidationError}


class TapirValidatorTranslationTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

  behavior of "Numeric translation"

  it should "translate numeric validators (including min/max)" in {
    import TapirValidatorTranslation.*
    translate( Validator.min[ Int ]( 5 ) ) shouldBe TapirValidator.Min[ Int ]( 5, false )
    translate( Validator.minExclusive[ Int ]( 5 ) ) shouldBe TapirValidator.Min[ Int ]( 5, true )
    translate( Validator.max[ Int ]( 5 ) ) shouldBe TapirValidator.Max[ Int ]( 5, false )
    translate( Validator.maxExclusive[ Int ]( 5 ) ) shouldBe TapirValidator.Max[ Int ]( 5, true )
    translate( Validator.positiveOrZero[ Int ] ) shouldBe TapirValidator.positiveOrZero[ Int ]
    translate( Validator.negativeOrZero[ Int ] )( 0 ) shouldBe Nil
    translate( Validator.negativeOrZero[ Int ] )( -1 ) shouldBe Nil
    translate( Validator.negativeOrZero[ Int ] )( 1 ) shouldBe List( ValidationError.Custom( 1, "value was not negative or zero", Nil ) )
  }

  it should "translate string validators" in {
    import TapirValidatorTranslation.translate
    translate( Validator.minLength( 5 ) ) shouldBe TapirValidator.minLength( 5 )
    translate( Validator.minLengthExclusive( 5 ) ) shouldBe TapirValidator.minLength( 6 )
    translate( Validator.maxLength( 5 ) ) shouldBe TapirValidator.maxLength( 5 )
    translate( Validator.maxLengthExclusive( 5 ) ) shouldBe TapirValidator.maxLength( 4 )
    translate( Validator.fixedLength( 5 ) ) shouldBe TapirValidator.fixedLength( 5 )
    translate( Validator.regex( "abcdefg" ) ) shouldBe TapirValidator.pattern( "abcdefg" )
    translate( Validator.regex( "abcdefg".r ) ) shouldBe TapirValidator.pattern( "abcdefg" )
  }

}
