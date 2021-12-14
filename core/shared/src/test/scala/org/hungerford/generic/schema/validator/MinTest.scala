package org.hungerford.generic.schema.validator

import org.scalatest.flatspec.AnyFlatSpecLike

class MinTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

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

  it should ""

}
