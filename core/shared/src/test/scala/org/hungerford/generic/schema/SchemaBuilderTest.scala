package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.ProductSchemaBuilder
import org.hungerford.generic.schema.validator.Validator
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import shapeless.HNil

class SchemaBuilderTest extends AnyFlatSpecLike with Matchers {

    object TestValidator extends Validator[ Int ] {
        override def isValid( instance : Int ) : Boolean = instance > 3
    }

    behavior of "SchemaBuilder"

    it should "be able to update description and validators" in {


        val schb = SchemaBuilder[ Int ]
          .description( "some description" )
          .validate( TestValidator )

        schb.vals shouldBe Set( TestValidator )
        schb.desc shouldBe Some( "some description" )
    }

    it should "be able to generate a primitive schema builder" in {
        val schb = SchemaBuilder[ Int ]
          .description( "some description" )
          .validate( TestValidator )
          .primitive
          .build

        schb shouldBe Primitive[ Int ]( Some( "some description" ), Set( TestValidator ) )

        val schb2 = SchemaBuilder[ Int ]
          .description( "some description" )
          .validate( TestValidator )
          .buildPrimitive

        schb2 shouldBe schb
    }

    it should "be able to generate a product schema builder" in {
        val schb = SchemaBuilder[ Int ]
          .description( "some description" )
          .validate( TestValidator )
          .product

        schb shouldBe ProductSchemaBuilder[ Int, HNil, HNil, Nothing, NoSchema.type ]( Some( "some description" ), Set( TestValidator ), None, HNil )
    }

    behavior of "PrimitiveSchemaBuilder"

    it should "be able to update description and validators" in {
        val schb = SchemaBuilder[ Int ]
          .primitive
          .description( "some description" )
          .validate( TestValidator )

        schb.vals shouldBe Set( TestValidator )
        schb.desc shouldBe Some( "some description" )

        schb.build shouldBe Primitive[ Int ]( Some( "some description" ), Set( TestValidator ) )
    }

}
