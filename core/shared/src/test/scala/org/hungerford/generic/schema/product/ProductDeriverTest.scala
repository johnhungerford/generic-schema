package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.{SchemaBuilder, NoSchema, Primitive, Schema, SchemaProvider}
import org.hungerford.generic.schema.product.field.{Field, FieldBuilder, UniqueFieldNames}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ProductDeriverTest extends AnyFlatSpecLike with Matchers {

    behavior of "ProductDeriver"

    case class Test( int : Int )

    it should "derive a ProductShape from a case class type using implicit primitives" in {

        import org.hungerford.generic.schema.primitives.Primitives.given

        val product = ProductDeriver[ Test ].derive

        product.construct( Tuple1( 5 ) ) shouldBe Test( 5 )

        product.size shouldBe 1

       product.fieldNames shouldBe Set( "int" )

       val manualProduct = SchemaBuilder[ Test ]
         .product
         .addField( FieldBuilder[ Int ].fieldName( "int" ).fromSchema.build )
         .construct( v => Test( v ) )
         .deconstruct( v => v.int )
         .build
         .shape

       product.fieldDescriptions shouldBe manualProduct.fieldDescriptions
    }

    it should "derive a ProductShape from a case class type by constructing primitives" in {
        val product = ProductDeriver[ Test ].derive

        product.construct( Tuple1( 5 ) ) shouldBe Test( 5 )

        product.size shouldBe 1

       product.fieldNames shouldBe Set( "int" )

       val manualProduct = SchemaBuilder[ Test ]
         .product
         .addField( FieldBuilder[ Int ].fieldName( "int" ).primitive.build )
         .construct( t => Test(t ) )
         .deconstruct( v => v.int )
         .build
         .shape

       product.fieldDescriptions shouldBe manualProduct.fieldDescriptions
    }

    it should "derive a big product" in {
        case class TestProduct( int : Int, str : String, bool : Boolean, double : Double, secondStr : String )

        val productSchema = ProductDeriver[ TestProduct ].derive

        productSchema.construct( ( 5, "hello", true, 0.23, "world" ) ) shouldBe TestProduct( 5, "hello", true, 0.23, "world" )
    }
}
