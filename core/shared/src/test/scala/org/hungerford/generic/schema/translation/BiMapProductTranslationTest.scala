package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.{ProductSchemaBuilder, ProductShape}
import org.hungerford.generic.schema.product.field.Field.AuxS
import org.hungerford.generic.schema.{NoSchema, Primitive, Schema, SchemaDeriver, SchemaProvider}
import org.hungerford.generic.schema.product.field.{Field, FieldBuilder, UniqueFieldNames}
import org.hungerford.generic.schema.product.translation.BiMapProductTranslation
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.Tuple.Concat
import scala.language.higherKinds
import scala.quoted.ToExpr.EmptyTupleToExpr

abstract class BiMapProductTranslationTest[ OtherSchema[ _ ], MapVal, BuildMapVal ](
   using
   intSch : OtherSchema[ Int ],
   strSch : OtherSchema[ String ],
   dblSch : OtherSchema[ Double ],
   boolSch : OtherSchema[ Boolean ],
) extends AnyFlatSpecLike
 with Matchers { this : BiMapProductTranslation[ OtherSchema, MapVal, BuildMapVal ] =>

   def writeJson[ T ]( value : T, schm : OtherSchema[ T ] ) : String

   import org.hungerford.generic.schema.Default.dsl.*

   it should "translate a product schema without additional fields" in {

       case class NoAF( intField : Int, strField : String )

       val testSchema = Schema.productBuilder[ NoAF ]
         .addField( FieldBuilder[ Int ].primitive.fieldName( "int_field" ).build )
         .addField( FieldBuilder[ String ].primitive.fieldName( "str_field" ).build )
         .construct( (int, str) => {
             NoAF( int, str )
         } )
         .deconstruct( value => (value.intField, value.strField) )
         .build

         val intExt = summon[ BiMapExtractor[ Int ] ]
         val strExt = summon[ BiMapExtractor[ String ] ]

//         val tupleExtr = summon[ BiMapTupleExtractor.Aux[ FieldDescription.Aux[ Int, intFieldName.type, Unit ] *: FieldDescription.AuxS[ String, strFieldName.type, Unit ] *:  EmptyTuple, Int *: String *: EmptyTuple ] ]

       val noAfRw = SchemaTranslator.translate( testSchema )

       writeJson( NoAF( 1, "hello" ), noAfRw ) shouldBe """{"int_field":1,"str_field":"hello"}"""
   }

   it should "translate a product schema with additional fields" in {
       case class HasAF( str : String, bool : Boolean, other : Map[ String, Double ] )

       val testSchema = Schema.productBuilder[ HasAF ]
         .additionalFields[ Double ].fromSchema( Schema.primitive )
         .addField( FieldBuilder[ String ].primitive.fieldName( "str_field" ).build )
         .addField( FieldBuilder[ Boolean ].primitive.fieldName( "bool_field" ).build )
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

        import org.hungerford.generic.schema.primitives.Primitives.given

        val testSchema = Schema.productBuilder[ HasAF ]
          .addField( FieldBuilder[ String ].fromSchema.fieldName( "str_field" ).build )
         .addField( FieldBuilder[ Boolean ].fromSchema.fieldName( "bool_field" ).build )
         .additionalFields[ Double ].fromSchema( Schema.primitive )
         .construct( (tup, af) => {
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

        import org.hungerford.generic.schema.primitives.Primitives.given

       val outsideSch = Schema.productBuilder[ Outside ]
         .addField(
             FieldBuilder[ Inside ]
               .fieldName( "inside_field" )
               .fromSchema( Schema.productBuilder[ Inside ].addField( FieldBuilder[ String ].fromSchema.fieldName( "str_field" ).build )
                 .construct( str => Inside( str ) )
                 .deconstruct( value => value.str )
                 .build
               )
               .build
         )
         .construct( inside => Outside( inside ) )
         .deconstruct( value => value.inside )
         .build

       val outsideRW : OtherSchema[ Outside ] = SchemaTranslator.translate( outsideSch )

       val testOutside = Outside( Inside( "hello" ) )

       writeJson( testOutside, outsideRW ) shouldBe """{"inside_field":{"str_field":"hello"}}"""
   }

   it should "be able to use nested product schemas through implicit resolution" in {
       case class Inside( str : String )
       case class Outside( inside : Inside )

        import org.hungerford.generic.schema.primitives.Primitives.given

       val insideSchema = Schema.productBuilder[ Inside ]
         .addField( FieldBuilder[ String ].fromSchema.fieldName( "str_field" ).build )
         .construct( str => {
             Inside( str ) 
          } )
         .deconstruct( value => value.str )
         .build

       val outsideSch = Schema.productBuilder[ Outside ]
         .addField( FieldBuilder[ Inside ].fromSchema( insideSchema ).fieldName( "inside_field" ).build )
         .construct( inside => {
            Outside( inside )
          } )
         .deconstruct( value => value.inside )
         .build

       implicit val outsideRW : OtherSchema[ Outside ] = SchemaTranslator.translate( outsideSch )

       val testOutside = Outside( Inside( "hello" ) )

       writeJson( testOutside, outsideRW ) shouldBe """{"inside_field":{"str_field":"hello"}}"""
   }

   it should "be able to translated nested product schemas provided by derivation" in {
       case class Inside( str : String )
       case class Outside( inside : Inside )

       import org.hungerford.generic.schema.primitives.Primitives.given

       val outsideSchema = SchemaProvider.schema[ Outside ]

       implicit val outsideRW : OtherSchema[ Outside ] = SchemaTranslator.translate( outsideSchema )

       val testOutside = Outside( Inside( "hello" ) )

       writeJson( testOutside, outsideRW ) shouldBe """{"inside":{"str":"hello"}}"""
   }

}
