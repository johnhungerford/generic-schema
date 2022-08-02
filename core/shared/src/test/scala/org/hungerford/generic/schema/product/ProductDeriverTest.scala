package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.{NoSchema, Primitive, Schema, SchemaProvider}
import org.hungerford.generic.schema.product.field.{Field, FieldBuilder, UniqueFieldNames}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ProductDeriverTest extends AnyFlatSpecLike with Matchers {

    behavior of "ProductDeriver"

    case class Test( int : Int )

    it should "derive a ProductShape from a case class type using implicit primitives" in {

        import org.hungerford.generic.schema.defaults.DefaultSchemas.given

        val product = ProductDeriver[ Test ].derive

        product.construct( Tuple1( 5 ) ) shouldBe Test( 5 )

        product.size shouldBe 1

        product.fieldNames shouldBe Set( "int" )

        val manualProduct = ProductSchemaBuilder[ Test ]
          .addField( FieldBuilder[ Test, Int ].name( "int" ).extractor( _.int ).fromSchema.build )
          .construct( v => Test( v ) )
          .build
          .shape

        product.fieldDescriptions.size shouldBe manualProduct.fieldDescriptions.size
        product.fieldDescriptions.head.fieldName shouldBe manualProduct.fieldDescriptions.head.fieldName
        product.fieldDescriptions.head.description shouldBe manualProduct.fieldDescriptions.head.description
        product.fieldDescriptions.head.extractor( Test( 25 ) ) shouldBe manualProduct.fieldDescriptions.head.extractor( Test( 25 ) )
    }

    it should "derive a ProductShape from a case class type by constructing primitives" in {
        val product = ProductDeriver[ Test ].derive

        product.construct( Tuple1( 5 ) ) shouldBe Test( 5 )

        product.size shouldBe 1

        product.fieldNames shouldBe Set( "int" )

        val manualProduct = ProductSchemaBuilder[ Test ]
          .addField( FieldBuilder[ Test, Int ].name( "int" ).extractor( _.int ).primitive.build )
          .construct( t => Test(t ) )
          .build
          .shape

        product.fieldDescriptions.size shouldBe manualProduct.fieldDescriptions.size
        product.fieldDescriptions.head.fieldName shouldBe manualProduct.fieldDescriptions.head.fieldName
        product.fieldDescriptions.head.description shouldBe manualProduct.fieldDescriptions.head.description
        product.fieldDescriptions.head.extractor( Test( 25 ) ) shouldBe manualProduct.fieldDescriptions.head.extractor( Test( 25 ) )
    }

    it should "derive a big product" in {
        case class TestProduct( int : Int, str : String, bool : Boolean, double : Double, secondStr : String )

        val productSchema = ProductDeriver[ TestProduct ].derive

        productSchema.construct( ( 5, "hello", true, 0.23, "world" ) ) shouldBe TestProduct( 5, "hello", true, 0.23, "world" )
    }
}
