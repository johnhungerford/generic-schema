package org.hungerford.generic.schema.bridge

import org.hungerford.generic.schema.product.ProductSchema
import org.hungerford.generic.schema.product.field.FieldDescription.Aux
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder}
import org.hungerford.generic.schema.{Primitive, SchemaBuilder}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import upickle.default._
import shapeless._

class BiMapProductSchemaBridgeTest extends AnyFlatSpecLike with Matchers {

    behavior of "BiMapProductSchemaBridge.Implicits.productTranslationWithoutAF"

    it should "translate a product schema without additional fields" in {

        case class NoAF( intField : Int, strField : String )

        implicit val testSchema = SchemaBuilder[ NoAF ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int_field" ).build )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str_field" ).build )
          .construct( (tup, _) => {
              val (int, str) = tup
              NoAF( int, str )
          } )
          .deconstruct( value => ((value.intField, value.strField), Map.empty) )
          .build

        import org.hungerford.generic.schema.upickle.UPickleSchemaTranslation._

        implicit val noAfRw: ReadWriter[ NoAF ] = rw

        write( NoAF( 1, "hello" ) ) shouldBe """{"int_field":1,"str_field":"hello"}"""
    }

    it should "translate a product schema with additional fields" in {
        case class HasAF( str : String, bool : Boolean, other : Map[ String, Double ] )

        implicit val testSchema = SchemaBuilder[ HasAF ]
          .product
          .additionalFields[ Double ].buildSchema( _.primitive.build )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str_field" ).build )
          .addField( FieldDescriptionBuilder[ Boolean ].primitive.fieldName( "bool_field" ).build )
          .construct( (tup, af : Map[ String, Double ]) => {
              val (str : String, bool : Boolean) = tup
              HasAF( str, bool, af )
          } )
          .deconstruct( value => ((value.str, value.bool), value.other) )
          .build

        import org.hungerford.generic.schema.upickle.UPickleSchemaTranslation._

        implicit val hasAFrw: ReadWriter[ HasAF ] = rw

        val res = write( HasAF( "hello", bool = true, Map( "test" -> 0.2, "test-2" -> 3.5 ) ) )
        res shouldBe """{"str_field":"hello","bool_field":true,"test":0.2,"test-2":3.5}"""
    }

    it should "use implicit primitive types" in {
        case class HasAF( str : String, bool : Boolean, other : Map[ String, Double ] )

        import org.hungerford.generic.schema.primitives._

        implicit val testSchema = SchemaBuilder[ HasAF ]
          .product
          .additionalFields[ Double ].buildSchema( _.primitive.build )
          .addField( FieldDescriptionBuilder[ String ].fromSchema.fieldName( "str_field" ).build )
          .addField( FieldDescriptionBuilder[ Boolean ].fromSchema.fieldName( "bool_field" ).build )
          .construct( (tup, af : Map[ String, Double ]) => {
              val (str : String, bool : Boolean) = tup
              HasAF( str, bool, af )
          } )
          .deconstruct( value => ((value.str, value.bool), value.other) )
          .build

        import org.hungerford.generic.schema.upickle.UPickleSchemaTranslation._

        implicit val hasAFrw: ReadWriter[ HasAF ] = rw

        val res = write( HasAF( "hello", bool = true, Map( "test" -> 0.2, "test-2" -> 3.5 ) ) )
        res shouldBe """{"str_field":"hello","bool_field":true,"test":0.2,"test-2":3.5}"""
    }

    it should "be able to use nested product schemas through nested building" in {
        case class Inside( str : String )
        case class Outside( inside : Inside )

        import org.hungerford.generic.schema.primitives._

        implicit val outsideSch = SchemaBuilder[ Outside ]
          .product
          .addField(
              FieldDescriptionBuilder[ Inside ]
                .fieldName( "inside_field" )
                .buildSchema( _.product
                  .addField( FieldDescriptionBuilder[ String ].fromSchema.fieldName( "str_field" ).build )
                  .construct( (tup, _) => Inside( tup._1 ) )
                  .deconstruct( value => (Tuple1( value.str ), Map.empty) )
                  .build
                )
                .build
          )
          .construct( (tup, _) => Outside( tup._1 ) )
          .deconstruct( value => (Tuple1( value.inside ), Map.empty ) )
          .build

        import org.hungerford.generic.schema.upickle.UPickleSchemaTranslation._

        implicit val outsideRW : ReadWriter[ Outside ] = rw

        val testOutside = Outside( Inside( "hello" ) )

        write( testOutside ) shouldBe """{"inside_field":{"str_field":"hello"}}"""
    }

    it should "be able to use nested product schemas through implicit resolution" in {
        case class Inside( str : String )
        case class Outside( inside : Inside )

        import org.hungerford.generic.schema.primitives._

        implicit val insideSch = SchemaBuilder[ Inside ]
          .product
          .addField( FieldDescriptionBuilder[ String ].fromSchema.fieldName( "str_field" ).build )
          .construct( (tup, _) => Inside( tup._1 ) )
          .deconstruct( value => (Tuple1( value.str ), Map.empty) )
          .build

        implicit val outsideSch = SchemaBuilder[ Outside ]
          .product
          .addField( FieldDescriptionBuilder[ Inside ].fromSchema.fieldName( "inside_field" ).build )
          .construct( (tup, _) => Outside( tup._1 ) )
          .deconstruct( value => (Tuple1( value.inside ), Map.empty ) )
          .build

        import org.hungerford.generic.schema.upickle.UPickleSchemaTranslation._

        implicit val outsideRW : ReadWriter[ Outside ] = rw

        val testOutside = Outside( Inside( "hello" ) )

        write( testOutside ) shouldBe """{"inside_field":{"str_field":"hello"}}"""

    }



}
