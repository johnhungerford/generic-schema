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

sealed trait RecursiveCoproduct
case object Term extends RecursiveCoproduct
final case class RecursiveProduct( b: RecursiveCoproduct, a: Int ) extends RecursiveCoproduct

object ProductTranslationTestSchemata {
    import generic.schema.exports.*

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
        import org.hungerford.generic.schema.defaults.DefaultSchemas.given

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
        import org.hungerford.generic.schema.defaults.DefaultSchemas.given

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
        import org.hungerford.generic.schema.defaults.DefaultSchemas.given

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
        import org.hungerford.generic.schema.defaults.DefaultSchemas.given

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
        import org.hungerford.generic.schema.defaults.DefaultSchemas.given
        SchemaProvider.schema[ Outside ]
    }

    val recursiveSchemaDerived = {
        import generic.schema.exports.*
        import org.hungerford.generic.schema.defaults.DefaultSchemas.given
        SchemaProvider.schema[ RecursiveProduct ]
    }

    val recursiveCoproductSch = {
        import generic.schema.exports.*
        recursiveSchemaDerived(t[ RecursiveCoproduct ]).schema
    }

    case class NestedProduct1( a: Int, b : Boolean, c : NestedProduct2 )
    case class NestedProduct2( d : NestedProduct3, e : Double, f : String )
    case class NestedProduct3( g : Char, h : NestedProduct4, i : Unit )
    case class NestedProduct4( j : Double, k : Int )

    val nestedProductSch = {
        import generic.schema.exports.{*, given}
        Schema.derived[NestedProduct1]
    }
}

import ProductTranslationTestSchemata.*

abstract class ProductJsonTranslationTest[ OtherSchema[ _ ] ](
    using
    intSch: OtherSchema[ Int ],
    strSch: OtherSchema[ String ],
    dblSch: OtherSchema[ Double ],
    boolSch: OtherSchema[ Boolean ],
) extends AnyFlatSpecLike with Matchers {

    def osNoAf: OtherSchema[ NoAF ]
    def osHasAf: OtherSchema[ HasAF ]
    def osHasAfPrim: OtherSchema[ HasAF ]
    def osOutside: OtherSchema[ Outside ]
    def osOutsideIns: OtherSchema[ Outside ]
    def osOutsideDer: OtherSchema[ Outside ]
    def osRecursiveSchemaDer: OtherSchema[ RecursiveProduct ]
    def osNested : OtherSchema[ NestedProduct1 ]

    def writeJson[ T ]( value: T, schm: OtherSchema[ T ] ): String
    def readJson[ T ]( value : String, schm: OtherSchema[ T ] ): T

    behavior of "ProductSchemaTranslator"

    it should "translate a product schema without additional fields" in {

        writeJson( NoAF( 1, "hello" ), osNoAf ) shouldBe """{"int_field":1,"str_field":"hello"}"""
        readJson[ NoAF ]( """{"int_field":1,"str_field":"hello"}""", osNoAf ) shouldBe NoAF( 1, "hello" )
    }

    it should "translate a product schema with additional fields" in {

        val correctJson = """{"str_field":"hello","bool_field":true,"test":0.2,"test-2":3.5}"""
        val value = HasAF( "hello", bool = true, Map( "test" -> 0.2, "test-2" -> 3.5 ) )
        writeJson( value, osHasAf ) shouldBe correctJson
        readJson[ HasAF ]( correctJson, osHasAf ) shouldBe value
    }

    it should "use implicit primitive types" in {
        val correctJson = """{"str_field":"hello","bool_field":true,"test":0.2,"test-2":3.5}"""
        val value = HasAF( "hello", bool = true, Map( "test" -> 0.2, "test-2" -> 3.5 ) )
        writeJson( value, osHasAfPrim ) shouldBe correctJson
        readJson[ HasAF ]( correctJson, osHasAfPrim ) shouldBe value
    }

    it should "be able to use nested product schemas through nested building" in {
        val correctJson = """{"inside_field":{"str_field":"hello"}}"""
        val value = Outside( Inside( "hello" ) )
        writeJson( value, osOutside ) shouldBe correctJson
        readJson[ Outside ]( correctJson, osOutside ) shouldBe value
    }

    it should "be able to use nested product schemas through implicit resolution" in {
        val correctJson = """{"inside_field":{"str_field":"hello"}}"""
        val value = Outside( Inside( "hello" ) )
        writeJson( value, osOutsideIns ) shouldBe correctJson
        readJson[ Outside ]( correctJson, osOutsideIns ) shouldBe value
    }

    it should "be able to translated nested product schemas provided by derivation" in {
        val correctJson = """{"inside":{"str":"hello"}}"""
        val value = Outside( Inside( "hello" ) )
        writeJson( value, osOutsideDer ) shouldBe correctJson
        readJson[ Outside ]( correctJson, osOutsideDer ) shouldBe value
    }

    it should "be able to translate a recursive product with lazy fields" in {
        val correctJson = """{"b":{"b":{"b":{"b":"Term","a":2},"a":3},"a":4},"a":5}"""
        val value = RecursiveProduct( RecursiveProduct( RecursiveProduct( RecursiveProduct( Term, 2 ), 3 ), 4 ), 5 )
        readJson[ RecursiveProduct ]( correctJson, osRecursiveSchemaDer ) shouldBe value
        writeJson( value, osRecursiveSchemaDer ) shouldBe correctJson
    }

    it should "be able to translate highly nested product types" in {
        val correctJson = """{"a":1,"b":true,"c":{"d":{"g":"i","h":{"j":0.23,"k":44},"i":{}},"e":23.04,"f":"hello"}}"""
        val value = NestedProduct1(1, true, NestedProduct2(NestedProduct3('i', NestedProduct4(0.23, 44), ()), 23.04, "hello"))
        readJson[ NestedProduct1 ]( correctJson, osNested ) shouldBe value
        writeJson( value, osNested ) shouldBe correctJson
    }

}
