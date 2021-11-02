package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.{Primitive, SchemaBuilder}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ProductSchemaBuilderTest extends AnyFlatSpecLike with Matchers {

    behavior of "ProductSchemaBuilder"

    it should "be able to add fields" in {
        SchemaBuilder[ Int ]
          .product
          .addField[ Double ](
              _.buildSchema( _.buildPrimitive )
                .fieldName( "some name" )
                .build
          )
          .addField[ String ](
              _.buildSchema( _.buildPrimitive )
                .fieldName( "some other name" )
                .build
          )
          .addField[ Boolean ](
              _.buildSchema( _.buildPrimitive )
                .description( "test description" )
                .fieldName( "bool" )
                .build
          )
    }

}
