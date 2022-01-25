package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.product.field.Field.Aux
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Default.dsl.*


class UniqueFieldNamesTest extends AnyFlatSpecLike with Matchers {

    behavior of "UniqueFieldNames"

    it should "be summoned for a tuple of one FieldDescription" in {
        summon[ UniqueFieldNames[ Field.Aux[ Int, Int, "hello", Unit ] *: EmptyTuple ] ]
    }

    it should "be summoned for a tuple of two unique FieldDescriptions" in {
        summon[ UniqueFieldNames[ Field.Aux[ Int, Int, "hello", Unit ] *: Field.Aux[ Int, Int, "there", Unit ] *: EmptyTuple ] ]
    }

    it should "not be summoned for a tuple of two FieldDescriptions with the same field name" in {
        assertDoesNotCompile( """summon[ UniqueFieldNames[ FieldDescription.Aux[ Int, Int, "hello", Unit ] *: FieldDescription.Aux[ Int, Int, "hello", Unit ] *: EmptyTuple ] ]""" )
    }

    it should "be summoned for field descriptions built in a schema" in {
        val sch = Schema.productBuilder[ (Int, String, Double) ]
            .addField( FieldBuilder[ (Int, String, Double), Int ].extractor( _._1 ).name( "name" ).primitive.build )
            .addField( FieldBuilder[ (Int, String, Double), String ].extractor( _._2 ).name( "is" ).primitive.build )
            .addField( FieldBuilder[ (Int, String, Double), Double ].extractor( _._3 ).name( "other" ).primitive.build )
            .construct( v => v )
            .build
            .shape

        summon[ UniqueFieldNames[ sch.R ] ]

        val sch2 = Schema.productBuilder[ (Int, String, Double) ]
            .addField( FieldBuilder[ (Int, String, Double), Int ].primitive.extractor( _._1 ).name( "name" ).build )
            .addField( FieldBuilder[ (Int, String, Double), String ].primitive.extractor( _._2 ).name( "is" ).build )
            .addField( FieldBuilder[ (Int, String, Double), Double ].primitive.extractor( _._3 ).name( "other" ).build )
            .construct( v => v )
            .build
            .shape

        summon[ UniqueFieldNames[ sch2.R ] ]

        val sch3 = Schema.productBuilder[ (Int, String, Double) ]
            .addField(
             FieldBuilder[ (Int, String, Double), Int ].primitive
               .name( "some name" )
               .extractor( _._1 )
               .build
            )
            .addField(
                FieldBuilder[ (Int, String, Double), String ].primitive
                  .extractor( _._2 )
                  .name( "some other name" )
                  .build
            )
            .addField(
                FieldBuilder[ (Int, String, Double), Double ].primitive
                  .description( "test description" )
                  .extractor( _._3 )
                  .name( "some other" )
                  .build
            )
            .construct( v => v )
            .build
            .shape

        // val shape = sch3.shape

        // summon[ UniqueFieldNames[ sch3.R ] ]
    }

    it should "do something else" in {
       case class NoAF( intField : Int, strField : String )

       val testSchema1 = Schema.productBuilder[ NoAF ]
         .addField( FieldBuilder[ NoAF, Int ].primitive.extractor( _.intField).name( "int_field" ).build )
         .addField( FieldBuilder[ NoAF, String ].primitive.extractor( _.strField).name( "str_field" ).build )
         .construct( (int, str) => {
             NoAF( int, str )
         } )
         .build

         val shape = testSchema1.shape

         val somat = summon[ UniqueFieldNames[ shape.R ] ]
    }

}
