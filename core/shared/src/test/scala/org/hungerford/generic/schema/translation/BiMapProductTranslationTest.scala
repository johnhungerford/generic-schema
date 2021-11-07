package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.{NoSchema, Primitive, Schema, SchemaBuilder, SchemaDeriver, SchemaProvider}
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.language.higherKinds

abstract class BiMapProductTranslationTest[ OtherSchema[ _ ], MapVal, BuildMapVal ](
    implicit
    intSch : OtherSchema[ Int ],
    strSch : OtherSchema[ String ],
    dblSch : OtherSchema[ Double ],
    boolSch : OtherSchema[ Boolean ],
) extends AnyFlatSpecLike
  with Matchers { this : BiMapProductTranslation[ OtherSchema, MapVal, BuildMapVal ] =>

    def writeJson[ T ]( value : T, schm : OtherSchema[ T ] ) : String

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

        val noAfRw: OtherSchema[ NoAF ] = SchemaTranslator.translate( testSchema )

        writeJson( NoAF( 1, "hello" ), noAfRw ) shouldBe """{"int_field":1,"str_field":"hello"}"""
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

        val hasAFrw: OtherSchema[ HasAF ] = SchemaTranslator.translate( testSchema )

        val res = writeJson( HasAF( "hello", bool = true, Map( "test" -> 0.2, "test-2" -> 3.5 ) ), hasAFrw )
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

        val hasAFrw: OtherSchema[ HasAF ] = SchemaTranslator.translate( testSchema )


        val res = writeJson( HasAF( "hello", bool = true, Map( "test" -> 0.2, "test-2" -> 3.5 ) ), hasAFrw )
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

        implicit val outsideRW : OtherSchema[ Outside ] = SchemaTranslator.translate( outsideSch )

        val testOutside = Outside( Inside( "hello" ) )

        writeJson( testOutside, outsideRW ) shouldBe """{"inside_field":{"str_field":"hello"}}"""
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

        implicit val outsideRW : OtherSchema[ Outside ] = SchemaTranslator.translate( outsideSch )

        val testOutside = Outside( Inside( "hello" ) )

        writeJson( testOutside, outsideRW ) shouldBe """{"inside_field":{"str_field":"hello"}}"""
    }

    it should "be able to translated nested product schemas provided by derivation" in {
        case class Inside( str : String )
        case class Outside( inside : Inside )

        import org.hungerford.generic.schema.primitives._

        implicit val outsideSchema = SchemaProvider.schema[ Outside ]

        implicit val outsideRW : OtherSchema[ Outside ] = SchemaTranslator.translate( outsideSchema )

        val testOutside = Outside( Inside( "hello" ) )

        writeJson( testOutside, outsideRW ) shouldBe """{"inside":{"str":"hello"}}"""
    }

}
