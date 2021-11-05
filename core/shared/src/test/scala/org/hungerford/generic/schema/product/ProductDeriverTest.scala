package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.{NoSchema, Primitive, Schema, SchemaBuilder}
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import shapeless._
import shapeless.labelled.FieldType
import org.hungerford.generic.schema.types.Provider

class ProductDeriverTest extends AnyFlatSpecLike with Matchers {

    behavior of "ProductDeriver"

    case class Test( int : Int )

    it should "derive a ProductShape from a case class type using implicit primitives" in {
        val lg = LabelledGeneric[ Test ]

        import org.hungerford.generic.schema.primitives._

        val product = ProductDeriver[ Test ].derive

        product.construct( Tuple1( 5 ), Map.empty[ String, Nothing ] ) shouldBe Test( 5 )

        product.size shouldBe 1

        product.fields shouldBe Set( "int" )

        val manualProduct = SchemaBuilder[ Test ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].fieldName( "int" ).fromSchema.build )
          .construct( (t, _) => Test(t._1) )
          .deconstruct( v => (Tuple1(v.int), Map.empty))
          .build
          .shape

        product.fieldDescriptions shouldBe manualProduct.fieldDescriptions
    }

    it should "derive a ProductShape from a case class type by constructing primitives" in {
        val lg = LabelledGeneric[ Test ]

        val product = ProductDeriver[ Test ].derive

        product.construct( Tuple1( 5 ), Map.empty[ String, Nothing ] ) shouldBe Test( 5 )

        product.size shouldBe 1

        product.fields shouldBe Set( "int" )

        val manualProduct = SchemaBuilder[ Test ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].fieldName( "int" ).primitive.build )
          .construct( (t, _) => Test(t._1) )
          .deconstruct( v => (Tuple1(v.int), Map.empty))
          .build
          .shape

        product.fieldDescriptions shouldBe manualProduct.fieldDescriptions
    }
}
