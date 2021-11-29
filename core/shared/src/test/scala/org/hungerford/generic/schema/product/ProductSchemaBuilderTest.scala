package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.FieldDescriptionBuilder
import org.hungerford.generic.schema.types.Provider
import org.hungerford.generic.schema.{Schema, Primitive, SchemaBuilder, SchemaProvider}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ProductSchemaBuilderTest extends AnyFlatSpecLike with Matchers {

    behavior of "ProductSchemaBuilder"

    case class TestCase( int : Int, str : String )

    it should "be able to add fields" in {
        SchemaBuilder[ Int ]
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
              FieldDescriptionBuilder[ Boolean ].buildSchema( _.buildPrimitive )
                .description( "test description" )
                .fieldName( "bool" )
                .build
          )
          .addField(
              FieldDescriptionBuilder[ BigInt ].buildSchema( _.buildPrimitive )
                .fieldName( "and another" )
                .build
          )
    }

    it should "be able to add constructor" in {
        SchemaBuilder[ TestCase ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .construct( ( tup, _ ) => {
              val (int, str) = tup
              TestCase( int, str )
          } )
    }

    it should "be able to add deconstructor" in {
        SchemaBuilder[ TestCase ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .deconstruct( ( value : TestCase ) => {
              ((value.int, value.str), Map.empty)
          } )
    }

    it should "be able to build schema if constructor and deconstructor are provided" in {
        SchemaBuilder[ TestCase ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .construct( ( tup, _ ) => {
              val (int, str) = tup
              TestCase( int, str )
          } )
          .deconstruct( ( value : TestCase ) => {
              ((value.int, value.str), Map.empty)
          } )
          .build
    }

    it should "be able to remove field descriptions" in {
        SchemaBuilder[ TestCase ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .addField( FieldDescriptionBuilder[ Boolean ].primitive.fieldName( "bool" ).build )
          .removeField( "bool" )
          .construct( ( tup, _ ) => {
              val (int, str) = tup
              TestCase( int, str )
          } )
          .deconstruct( ( value : TestCase ) => {
              ((value.int, value.str), Map.empty)
          } )
          .build

        SchemaBuilder[ TestCase ]
          .product
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .addField( FieldDescriptionBuilder[ Boolean ].primitive.fieldName( "bool" ).build )
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .removeField( "bool" )
          .removeField( "str" )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .construct( ( tup, _ ) => {
              val (int, str) = tup
              TestCase( int, str )
          } )
          .deconstruct( ( value : TestCase ) => {
              ((value.int, value.str), Map.empty)
          } )
          .build
    }

    it should "be able to rebuild" in {
        val schema = SchemaBuilder[ TestCase ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .construct( ( tup, _ ) => {
              val (int, str) = tup
              TestCase( int, str )
          } )
          .deconstruct( ( value : TestCase ) => {
              ((value.int, value.str), Map.empty)
          } )
          .build

        ProductSchemaBuilder.from( schema )
          .removeField( "str" )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "string" ).build )
          .construct( ( tup, _ ) => {
              val (int, str) = tup
              TestCase( int, str )
          } )
          .deconstruct( ( value : TestCase ) => {
              ((value.int, value.str), Map.empty)
          } )
          .build

    }

//    it should "be able to update fields without replacing them and losing constructor/deconstructor" in {
//        val schema = SchemaBuilder[ TestCase ]
//          .caseClass
//          .updateField( "int" )( _.fieldName( "int_field" ).build )
//          .build
//
//        schema.shape.fieldDescriptions.head.fieldName shouldBe "int_field"
//    }

}
