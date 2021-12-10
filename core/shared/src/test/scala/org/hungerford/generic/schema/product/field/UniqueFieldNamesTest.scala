package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.product.field.Field.Aux
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Default.dsl.*


class UniqueFieldNamesTest extends AnyFlatSpecLike with Matchers {

    behavior of "UniqueFieldNames"

    it should "be summoned for a tuple of one FieldDescription" in {
        summon[ UniqueFieldNames[ Field.Aux[ Int, "hello", Unit ] *: EmptyTuple ] ]
    }

    it should "be summoned for a tuple of two unique FieldDescriptions" in {
        summon[ UniqueFieldNames[ Field.Aux[ Int, "hello", Unit ] *: Field.Aux[ Int, "there", Unit ] *: EmptyTuple ] ]
    }

    it should "not be summoned for a tuple of two FieldDescriptions with the same field name" in {
        assertDoesNotCompile( """summon[ UniqueFieldNames[ FieldDescription.Aux[ Int, "hello", Unit ] *: FieldDescription.Aux[ Int, "hello", Unit ] *: EmptyTuple ] ]""" )
    }

    it should "be summoned for field descriptions built in a schema" in {
        val sch = Schema.productBuilder[ (Int, String, Double) ]
            .addField( FieldBuilder[ Int ].fieldName( "name" ).primitive.build )
            .addField( FieldBuilder[ String ].fieldName( "is" ).primitive.build )
            .addField( FieldBuilder[ Double ].fieldName( "other" ).primitive.build )
            .construct( v => v )
            .deconstruct( v => v )
            .build
            .shape

        summon[ UniqueFieldNames[ sch.R ] ]

        val sch2 = Schema.productBuilder[ (Int, String, Double) ]
            .addField( FieldBuilder[ Int ].primitive.fieldName( "name" ).build )
            .addField( FieldBuilder[ String ].primitive.fieldName( "is" ).build )
            .addField( FieldBuilder[ Double ].primitive.fieldName( "other" ).build )
            .construct( v => v )
            .deconstruct( v => v )
            .build
            .shape

        summon[ UniqueFieldNames[ sch2.R ] ]

        val sch3 = Schema.productBuilder[ (Int, String, Double) ]
            .addField(
             FieldBuilder[ Int ].primitive
               .fieldName( "some name" )
               .build
            )
            .addField(
                FieldBuilder[ String ].primitive
                  .fieldName( "some other name" )
                  .build
            )
            .addField(
                FieldBuilder[ Double ].primitive
                  .description( "test description" )
                  .fieldName( "some other" )
                  .build
            )
            .construct( v => v )
            .deconstruct( v => v )
            .build
            .shape

        // val shape = sch3.shape

        // summon[ UniqueFieldNames[ sch3.R ] ]
    }

    it should "do something else" in {
       case class NoAF( intField : Int, strField : String )

       val testSchema1 = Schema.productBuilder[ NoAF ]
         .addField( FieldBuilder[ Int ].primitive.fieldName( "int_field" ).build )
         .addField( FieldBuilder[ String ].primitive.fieldName( "str_field" ).build )
         .construct( (int, str) => {
             NoAF( int, str )
         } )
         .deconstruct( value => (value.intField, value.strField) )
         .build

         val shape = testSchema1.shape

         val somat = summon[ UniqueFieldNames[ shape.R ] ]
    }

}
