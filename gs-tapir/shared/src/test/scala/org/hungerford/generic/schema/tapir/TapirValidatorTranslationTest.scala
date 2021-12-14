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

  it should "translate collections validators" in {
    import TapirValidatorTranslation.translate
    translate( Validator.minSize[ Int, List ]( 5 ) ) shouldBe TapirValidator.minSize[ Int, List ]( 5 )
    translate( Validator.minSizeExclusive[ Int, LazyList ]( 5 ) ) shouldBe TapirValidator.minSize[ Int, LazyList ]( 6 )
    translate( Validator.maxSize[ Int, Iterable ]( 5 ) ) shouldBe TapirValidator.maxSize[ Int, Iterable ]( 5 )
    translate( Validator.maxSizeExclusive[ Int, Seq ]( 5 ) ) shouldBe TapirValidator.maxSize[ Int, Seq ]( 4 )
    translate( Validator.fixedSize[ Int, Set ]( 5 ) ) shouldBe TapirValidator.fixedSize[ Int, Set ]( 5 )
  }

  it should "translate enum validators" in {
    import TapirValidatorTranslation.translate
    translate( Validator.oneOf( "a", "b", "c" ) ) shouldBe TapirValidator.enumeration( List( "a", "b", "c" ) )
    translate( Validator.noneOf( "a", "b", "c" ) )( "d" ) shouldBe Nil
    translate( Validator.noneOf( "a", "b", "c" ) )( "a" ) shouldBe List( ValidationError.Custom( "a", "value is excluded" ) )
  }

}
