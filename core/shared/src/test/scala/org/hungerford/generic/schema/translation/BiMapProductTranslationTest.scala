package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.FieldDescription.AuxS
import org.hungerford.generic.schema.{NoSchema, Primitive, Schema, SchemaBuilder, SchemaDeriver, SchemaProvider}
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder, UniqueFieldNames}
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

   it should "translate a product schema without additional fields" in {

       case class NoAF( intField : Int, strField : String )

       val testSchema = SchemaBuilder[ NoAF ]
         .product
         .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int_field" ).build )
         .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str_field" ).build )
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

       val testSchema = SchemaBuilder[ HasAF ]
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

        import org.hungerford.generic.schema.primitives.Primitives.given

        val testSchema = SchemaBuilder[ HasAF ]
         .product
         .addField( FieldDescriptionBuilder[ String ].fromSchema.fieldName( "str_field" ).build )
         .addField( FieldDescriptionBuilder[ Boolean ].fromSchema.fieldName( "bool_field" ).build )
         .additionalFields[ Double ].buildSchema( _.primitive.build )
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

       val outsideSch = SchemaBuilder[ Outside ]
         .product
         .addField(
             FieldDescriptionBuilder[ Inside ]
               .fieldName( "inside_field" )
               .buildSchema( _.product
                 .addField( FieldDescriptionBuilder[ String ].fromSchema.fieldName( "str_field" ).build )
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

       val insideSchema = SchemaBuilder[ Inside ]
         .product
         .addField( FieldDescriptionBuilder[ String ].fromSchema.fieldName( "str_field" ).build )
         .construct( str => {
             Inside( str ) 
          } )
         .deconstruct( value => value.str )
         .build

       val outsideSch = SchemaBuilder[ Outside ]
         .product
         .addField( FieldDescriptionBuilder[ Inside ].fromSchema( insideSchema ).fieldName( "inside_field" ).build )
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
