package org.hungerford.generic.schema.validator

import org.scalatest.flatspec.AnyFlatSpecLike

class MinMaxValidatorsTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

  behavior of "Min"

  it should "be constructable for integers" in {
    val validator = Validator.min[ Int ]( 5 )
    validator.isValid( 0 ) shouldBe false
    validator.isValid( 20 ) shouldBe true
  }

  it should "be constructable for doubles" in {
    val validator = Validator.min[ Double ]( 0.5 )
    validator.isValid( 0.0000034245432 ) shouldBe false
    validator.isValid( 23.234 ) shouldBe true
  }

  it should "be constructable for big int" in {
    val validator = Validator.min[ BigInt ]( BigInt( "734435673895382736473" ) )
    validator.isValid( BigInt( "3443567389538273673" ) ) shouldBe false
    validator.isValid( BigInt( "89238934734435673895382736473" ) ) shouldBe true
  }

  it should "be constructable for string" in {
    val validator = Validator.min[ String ]( "elephant" )
    validator.isValid( "electricity" ) shouldBe false
    validator.isValid( "elequant" ) shouldBe true
  }

  it should "validate a value equal to the min value if the exclusive parameter is false" in {
    val validator = Validator.min[ Int ]( 5 )
    validator.isValid( 5 ) shouldBe true
  }

  it should "not validate a value equal to the min value if the exclusive parameter is true" in {
    val validator = Validator.exclusiveMin[ Int ]( 5 )
    validator.isValid( 5 ) shouldBe false
  }

  behavior of "Max"

  it should "be constructable for integers" in {
    val validator = Validator.max[ Int ]( 5 )
    validator.isValid( 0 ) shouldBe true
    validator.isValid( 20 ) shouldBe false
  }

  it should "be constructable for doubles" in {
    val validator = Validator.max[ Double ]( 0.5 )
    validator.isValid( 0.0000034245432 ) shouldBe true
    validator.isValid( 23.234 ) shouldBe false
  }

  it should "be constructable for big int" in {
    val validator = Validator.max[ BigInt ]( BigInt( "734435673895382736473" ) )
    validator.isValid( BigInt( "3443567389538273673" ) ) shouldBe true
    validator.isValid( BigInt( "89238934734435673895382736473" ) ) shouldBe false
  }

  it should "be constructable for string" in {
    val validator = Validator.max[ String ]( "elephant" )
    validator.isValid( "electricity" ) shouldBe true
    validator.isValid( "elequant" ) shouldBe false
  }

  it should "validate a value equal to the max value if the exclusive parameter is false" in {
    val validator = Validator.max[ Int ]( 5 )
    validator.isValid( 5 ) shouldBe true
  }

  it should "not validate a value equal to the max value if the exclusive parameter is true" in {
    val validator = Validator.exclusiveMax[ Int ]( 5 )
    validator.isValid( 5 ) shouldBe false
  }
  
}
