package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.product.field.FieldDescription.Aux
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.hungerford.generic.schema.SchemaBuilder


class UniqueFieldNamesTest extends AnyFlatSpecLike with Matchers {

    behavior of "UniqueFieldNames"

    it should "be summoned for a tuple of one FieldDescription" in {
        summon[ UniqueFieldNames[ FieldDescription.Aux[ Int, "hello", Unit ] *: EmptyTuple ] ]
    }

    it should "be summoned for a tuple of two unique FieldDescriptions" in {
        summon[ UniqueFieldNames[ FieldDescription.Aux[ Int, "hello", Unit ] *: FieldDescription.Aux[ Int, "there", Unit ] *: EmptyTuple ] ]
    }

    it should "not be summoned for a tuple of two FieldDescriptions with the same field name" in {
        assertDoesNotCompile( """summon[ UniqueFieldNames[ FieldDescription.Aux[ Int, "hello", Unit ] *: FieldDescription.Aux[ Int, "hello", Unit ] *: EmptyTuple ] ]""" )
    }

    it should "be summoned for field descriptions built in a schema" in {
        val sch = SchemaBuilder[ (Int, String, Double) ]
            .product
            .addField( FieldDescriptionBuilder[ Int ].fieldName( "name" ).primitive.build )
            .addField( FieldDescriptionBuilder[ String ].fieldName( "is" ).primitive.build )
            .addField( FieldDescriptionBuilder[ Double ].fieldName( "other" ).primitive.build )
            .construct( (tup, _) => tup )
            .deconstruct( v => (v, Map.empty) )
            .build
            .shape

        summon[ UniqueFieldNames[ sch.R ] ]

        val sch2 = SchemaBuilder[ (Int, String, Double) ]
            .product
            .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "name" ).build )
            .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "is" ).build )
            .addField( FieldDescriptionBuilder[ Double ].primitive.fieldName( "other" ).build )
            .construct( (tup, _) => tup )
            .deconstruct( v => (v, Map.empty) )
            .build
            .shape

        summon[ UniqueFieldNames[ sch2.R ] ]

        val sch3 = SchemaBuilder[ (Int, String, Double) ]
            .product
            .addField(
             FieldDescriptionBuilder[ Int ].buildSchema( _.buildPrimitive )
               .fieldName( "some name" )
               .build
            )
            .addField(
                FieldDescriptionBuilder[ String ].buildSchema( _.buildPrimitive )
                  .fieldName( "some other name" )
                  .build
            )
            .addField(
                FieldDescriptionBuilder[ Double ].buildSchema( _.buildPrimitive )
                  .description( "test description" )
                  .fieldName( "some other" )
                  .build
            )
            .construct( (tup, _) => tup )
            .deconstruct( v => (v, Map.empty) )
            .build
            .shape

        // val shape = sch3.shape

        // summon[ UniqueFieldNames[ sch3.R ] ]
    }

    it should "do something else" in {
       case class NoAF( intField : Int, strField : String )

       val testSchema1 = SchemaBuilder[ NoAF ]
         .product
         .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int_field" ).build )
         .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str_field" ).build )
         .construct( (tup, _) => {
             val (int, str) = tup
             NoAF( int, str )
         } )
         .deconstruct( value => ((value.intField, value.strField), Map.empty) )
         .build

         val shape = testSchema1.shape

         val somat = summon[ UniqueFieldNames[ shape.R ] ]
    }

}
