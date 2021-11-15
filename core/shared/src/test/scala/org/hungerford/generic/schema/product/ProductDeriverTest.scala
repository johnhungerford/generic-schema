package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.{NoSchema, Primitive, Schema, SchemaProvider}
import org.hungerford.generic.schema.product.field.FieldDescription
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.hungerford.generic.schema.types.Provider

class ProductDeriverTest extends AnyFlatSpecLike with Matchers {

    behavior of "ProductDeriver"

    case class Test( int : Int )

    given intSchema : SchemaProvider[ Int ] = new SchemaProvider[ Int ] {
        type Shape = Unit

        def provide : Schema.Aux[ Int, Unit ] = Primitive[ Int ]()
    }

    it should "derive a ProductShape from a case class type using implicit primitives" in {

        summon[ Test <:< Product ]

        val product = ProductDeriver[ Test ].derive

        product.construct( Tuple1( 5 ), Map.empty[ String, Nothing ] ) shouldBe Test( 5 )

        product.size shouldBe 1

//        product.fields shouldBe Set( "int" )

//        val manualProduct = SchemaBuilder[ Test ]
//          .product
//          .addField( FieldDescriptionBuilder[ Int ].fieldName( "int" ).fromSchema.build )
//          .construct( (t, _) => Test(t._1) )
//          .deconstruct( v => (Tuple1(v.int), Map.empty))
//          .build
//          .shape
//
//        product.fieldDescriptions shouldBe manualProduct.fieldDescriptions
    }

    it should "derive a ProductShape from a case class type by constructing primitives" in {
        val product = ProductDeriver[ Test ].derive

        product.construct( Tuple1( 5 ) ) shouldBe Test( 5 )

        product.size shouldBe 1

//        product.fields shouldBe Set( "int" )

//        val manualProduct = SchemaBuilder[ Test ]
//          .product
//          .addField( FieldDescriptionBuilder[ Int ].fieldName( "int" ).primitive.build )
//          .construct( (t, _) => Test(t._1) )
//          .deconstruct( v => (Tuple1(v.int), Map.empty))
//          .build
//          .shape
//
//        product.fieldDescriptions shouldBe manualProduct.fieldDescriptions
    }

    it should "derive a big product" in {
        case class TestProduct( int : Int, str : String, bool : Boolean, double : Double, secondStr : String )

        val productSchema = ProductDeriver[ TestProduct ].derive

        productSchema.construct( ( 5, "hello", true, 0.23, "world" ) ) shouldBe TestProduct( 5, "hello", true, 0.23, "world" )
    }
}
