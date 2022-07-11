package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.{ProductSchemaBuilder, ProductShape}
import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.{NoSchema, Primitive, Schema, SchemaDeriver, SchemaProvider}
import org.hungerford.generic.schema.product.field.{Field, FieldBuilder, UniqueFieldNames}
import org.hungerford.generic.schema.product.translation.{BiMapProductTranslation, Decoder, Encoder}
import org.hungerford.generic.schema.translation.ProductTranslationTestSchemata.NoAF
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.Tuple.Concat
import scala.util.NotGiven
import scala.quoted.ToExpr.EmptyTupleToExpr


object ProductTranslationTestSchemata {
    import org.hungerford.generic.schema.Default.dsl.*

    case class NoAF( intField: Int, strField: String )

    val noAfSchema = Schema.productBuilder[ NoAF ]
      .addField( FieldBuilder[ NoAF, Int ].primitive.name( "int_field" ).extractor( _.intField ).build )
      .addField( FieldBuilder[ NoAF, String ].primitive.name( "str_field" ).extractor( _.strField ).build )
      .construct(
          ( int, str ) => {
              NoAF( int, str )
          }
      )
      .build

    case class HasAF( str: String, bool: Boolean, other: Map[ String, Double ] )

    val hasAfSchema = Schema.productBuilder[ HasAF ]
      .additionalFields[ Double ].fromSchema( _.other )( Schema.primitive )
      .addField( FieldBuilder[ HasAF, String ].primitive.name( "str_field" ).extractor( _.str ).build )
      .addField( FieldBuilder[ HasAF, Boolean ].primitive.name( "bool_field" ).extractor( _.bool ).build )
      .construct(
          ( tup, af: Map[ String, Double ] ) => {
              val (str: String, bool: Boolean) = tup
              HasAF( str, bool, af )
          }
       )
      .build

    val hasAfPrimitiveSch = {
        import org.hungerford.generic.schema.primitives.Primitives.given

        Schema.productBuilder[ HasAF ]
          .addField( FieldBuilder[ HasAF, String ].fromSchema.name( "str_field" ).extractor( _.str ).build )
          .addField( FieldBuilder[ HasAF, Boolean ].fromSchema.name( "bool_field" ).extractor( _.bool ).build )
          .additionalFields[ Double ].fromSchema( _.other )( Schema.primitive )
          .construct(
              ( tup, af ) => {
                  val (str: String, bool: Boolean) = tup
                  HasAF( str, bool, af )
              }
          )
          .build
    }

    case class Inside( str: String )
    case class Outside( inside: Inside )

    val outsideSch = {
        import org.hungerford.generic.schema.primitives.Primitives.given

        Schema.productBuilder[ Outside ]
          .addField(
              FieldBuilder[ Outside, Inside ]
                .name( "inside_field" )
                .extractor( _.inside )
                .fromSchema(
                    Schema.productBuilder[ Inside ].addField(
                        FieldBuilder[ Inside, String ].extractor( _.str ).fromSchema.name( "str_field" ).build
                       )
                      .construct( str => Inside( str ) )
                      .build
                    )
                .build
              )
          .construct( inside => Outside( inside ) )
          .build
    }

    val insideSchema = {
        import org.hungerford.generic.schema.primitives.Primitives.given

        Schema.productBuilder[ Inside ]
          .addField( FieldBuilder[ Inside, String ].extractor( _.str ).fromSchema.name( "str_field" ).build )
          .construct(
              str => {
                  Inside( str )
              }
           )
          .build
    }

    val outsideSchUsingInside = {
        import org.hungerford.generic.schema.primitives.Primitives.given

        Schema.productBuilder[ Outside ]
          .addField( FieldBuilder[ Outside, Inside ].extractor( _.inside ).fromSchema( insideSchema ).name( "inside_field" ).build )
          .construct(
              inside => {
                  Outside( inside )
              }
          )
          .build
    }

    val outsideSchemaDerived = {
        import org.hungerford.generic.schema.primitives.Primitives.given
        SchemaProvider.schema[ Outside ]
    }
}

import ProductTranslationTestSchemata.*

abstract class ProductJsonTranslationTest[ OtherSchema[ _ ] ](
    using
    intSch: OtherSchema[ Int ],
    strSch: OtherSchema[ String ],
    dblSch: OtherSchema[ Double ],
    boolSch: OtherSchema[ Boolean ],
    transNoAf: SchemaTranslator[ NoAF, noAfSchema.Shape, OtherSchema ],
    trnasHasAf: SchemaTranslator[ HasAF, hasAfSchema.Shape, OtherSchema ],
    transHasAfPrim: SchemaTranslator[ HasAF, hasAfPrimitiveSch.Shape, OtherSchema ],
    transOutside: SchemaTranslator[ Outside, outsideSch.Shape, OtherSchema ],
    transInside: SchemaTranslator[ Inside, insideSchema.Shape, OtherSchema ],
    transOutsideIns: SchemaTranslator[ Outside, outsideSchUsingInside.Shape, OtherSchema ],
    transOutsideDer: SchemaTranslator[ Outside, outsideSchemaDerived.Shape, OtherSchema ],
) extends AnyFlatSpecLike with Matchers {

    def writeJson[ T ]( value: T, schm: OtherSchema[ T ] ): String
    
    behavior of "ProductSchemaTranslator"

    it should "translate a product schema without additional fields" in {

        val noAfRw = SchemaTranslator.translate( noAfSchema )

        writeJson( NoAF( 1, "hello" ), noAfRw ) shouldBe """{"int_field":1,"str_field":"hello"}"""
    }

    it should "translate a product schema with additional fields" in {

        val hasAFrw: OtherSchema[ HasAF ] = SchemaTranslator.translate( hasAfSchema )( using trnasHasAf )

        val res = writeJson( HasAF( "hello", bool = true, Map( "test" -> 0.2, "test-2" -> 3.5 ) ), hasAFrw )
        res shouldBe """{"str_field":"hello","bool_field":true,"test":0.2,"test-2":3.5}"""
    }

    it should "use implicit primitive types" in {
        val hasAFrw: OtherSchema[ HasAF ] = SchemaTranslator.translate( hasAfPrimitiveSch )( using transHasAfPrim )

        val res = writeJson( HasAF( "hello", bool = true, Map( "test" -> 0.2, "test-2" -> 3.5 ) ), hasAFrw )
        res shouldBe """{"str_field":"hello","bool_field":true,"test":0.2,"test-2":3.5}"""
    }

    it should "be able to use nested product schemas through nested building" in {

        val outsideRW: OtherSchema[ Outside ] = SchemaTranslator.translate( outsideSch )( using transOutside )

        val testOutside = Outside( Inside( "hello" ) )

        writeJson( testOutside, outsideRW ) shouldBe """{"inside_field":{"str_field":"hello"}}"""
    }

    it should "be able to use nested product schemas through implicit resolution" in {
        implicit val outsideRW: OtherSchema[ Outside ] = SchemaTranslator.translate( outsideSchUsingInside )( using transOutsideIns )

        val testOutside = Outside( Inside( "hello" ) )

        writeJson( testOutside, outsideRW ) shouldBe """{"inside_field":{"str_field":"hello"}}"""
    }

    it should "be able to translated nested product schemas provided by derivation" in {
        implicit val outsideRW: OtherSchema[ Outside ] = SchemaTranslator.translate( outsideSchemaDerived )( using transOutsideDer )

        val testOutside = Outside( Inside( "hello" ) )

        writeJson( testOutside, outsideRW ) shouldBe """{"inside":{"str":"hello"}}"""
    }

}
