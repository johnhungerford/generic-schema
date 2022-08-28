package org.hungerford.generic.schema.validator

import org.scalatest.flatspec.AnyFlatSpecLike

class CollectionValidatorsTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

  behavior of "CollSize"

  it should "validate based on minimum size inclusively" in {
    def validator[ Col : Collection ] = Validator.minSize[ Col ]( 5 )
    validator.isValid( "1234" ) shouldBe false
    validator.isValid( Array( 1, 2, 3, 4 ) ) shouldBe false
    validator.isValid( "12345" ) shouldBe true
    validator.isValid( Array( 1, 2, 3, 4, 5 ) ) shouldBe true
    validator.isValid( "123456" ) shouldBe true
    validator.isValid( Array( 1, 2, 3, 4, 5 ) ) shouldBe true
  }

  it should "validate based on minimum size exclusively" in {
    def validator[ Col : Collection ] = Validator.minSizeExclusive[ Col ]( 5 )
    validator.isValid( "1234" ) shouldBe false
    validator.isValid( Array( 1, 2, 3, 4 ) ) shouldBe false
    validator.isValid( "12345" ) shouldBe false
    validator.isValid( Array( 1, 2, 3, 4, 5 ) ) shouldBe false
    validator.isValid( "123456" ) shouldBe true
    validator.isValid( Array( 1, 2, 3, 4, 5, 6 ) ) shouldBe true
  }

  it should "validate based on maximum size inclusively" in {
    def validator[ Col : Collection ] = Validator.maxSize[ Col ]( 5 )
    validator.isValid( "1234" ) shouldBe true
    validator.isValid( Array( 1, 2, 3, 4 ) ) shouldBe true
    validator.isValid( "12345" ) shouldBe true
    validator.isValid( Array( 1, 2, 3, 4, 5 ) ) shouldBe true
    validator.isValid( "123456" ) shouldBe false
    validator.isValid( Array( 1, 2, 3, 4, 5, 6 ) ) shouldBe false
  }

  it should "validate based on maximum size exclusively" in {
    def validator[ Col : Collection ] = Validator.maxSizeExclusive[ Col ]( 5 )
    validator.isValid( "1234" ) shouldBe true
    validator.isValid( Array( 1, 2, 3, 4 ) ) shouldBe true
    validator.isValid( "12345" ) shouldBe false
    validator.isValid( Array( 1, 2, 3, 4, 5 ) ) shouldBe false
    validator.isValid( "123456" ) shouldBe false
    validator.isValid( Array( 1, 2, 3, 4, 5, 6 ) ) shouldBe false
  }

  it should "validate based on exact size" in {
    def validator[ Col : Collection ] = Validator.fixedSize[ Col ]( 5 )
    validator.isValid( "1234" ) shouldBe false
    validator.isValid( Array( 1, 2, 3, 4 ) ) shouldBe false
    validator.isValid( "12345" ) shouldBe true
    validator.isValid( Array( 1, 2, 3, 4, 5 ) ) shouldBe true
    validator.isValid( "123456" ) shouldBe false
    validator.isValid( Array( 1, 2, 3, 4, 5, 6 ) ) shouldBe false
  }

  it should "correctly validate a non-empty iterable" in {
    def validator[ Col : Collection ] = Validator.nonEmptyCollection[ Col ]
    validator.isValid( "" ) shouldBe false
    validator.isValid( Array[ Int ]() ) shouldBe false
    validator.isValid( "." ) shouldBe true
    validator.isValid( Array( 0 ) ) shouldBe true
    validator.isValid( "safgergwq3r4t35rtgewfuwri9g8oejih4uovfnsorgihw94" ) shouldBe true
    validator.isValid( Array( 0, 1, 2, 3, 4, 5, 6, 7, 3452452, 324524634, 34534, 869648 ) ) shouldBe true
  }

}
