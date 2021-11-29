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
          .construct( (int, str) => TestCase( int, str ) )
    }

    it should "be able to add deconstructor" in {
        SchemaBuilder[ TestCase ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
    }

    it should "be able to build schema if constructor and deconstructor are provided" in {
        SchemaBuilder[ TestCase ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
          .build
    }

    it should "be able to remove field descriptions" in {
        SchemaBuilder[ TestCase ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .addField( FieldDescriptionBuilder[ Boolean ].primitive.fieldName( "bool" ).build )
          .removeField( "bool" )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
          .build

        SchemaBuilder[ TestCase ]
          .product
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .addField( FieldDescriptionBuilder[ Boolean ].primitive.fieldName( "bool" ).build )
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .removeField( "bool" )
          .removeField( "str" )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
          .build
    }

    it should "be able to rebuild" in {
        val schema = SchemaBuilder[ TestCase ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
          .build

        ProductSchemaBuilder.from( schema )
          .removeField( "str" )
          .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "string" ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
          .build

    }

    it should "be able to add fields after adding constructor/deconstructor, but require that you rebuild the constructor/deconstructor" in {
      val builder = SchemaBuilder[ TestCase ]
        .product
        .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
        .addField( FieldDescriptionBuilder[ String ].primitive.fieldName( "str" ).build )
        .construct( (int, str) => TestCase( int, str ) )
        .deconstruct( v => (v.int, v.str) )
      
      assertCompiles( """builder.build""" )

      val newBuilder = builder.addField( FieldDescriptionBuilder[ Boolean ].primitive.fieldName( "bool" ).build )

      assertDoesNotCompile( """newBuilder.build""")

      newBuilder
        .construct( (int, str, _) => TestCase( int, str ) )
        .deconstruct( v => (v.int, v.str, false) )
        .build
    }

    it should "preserve constructor/deconstructor when updating additional fields to same type, but does not preserve when additional fields type is changed" in {
        case class TC( int : Int, fields : Map[ String, String ] )

        val builder = SchemaBuilder[ TC ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].primitive.fieldName( "int" ).build )
          .additionalFields[ String ].buildSchema( _.primitive.build )
          .construct( (int, af) => TC( int, af ) )
          .deconstruct( v => (v.int, v.fields) )
        
        assertCompiles( """builder.build""" )

        import org.hungerford.generic.schema.primitives.Primitives.given

        val newBuilder = builder
          .additionalFields[ String ].fromSchema

        assertCompiles( """newBuilder.build""" )

        val lastBuilder = builder
          .additionalFields[ Double ].fromSchema
        
        assertDoesNotCompile( """lastBuilder.build""")

        lastBuilder
          .construct( (int, af) => TC( int, af.mapValues( _.toString ).toMap ) )
          .deconstruct( v => (v.int, v.fields.mapValues( _.toDouble ).toMap ) )
          .build
    }

    it should "be able to rebuild a field, selected by name" in {
        val sch = SchemaBuilder[ TestCase ]
          .caseClass
          .updateField( "int" )( _.fieldName( "int_field" ).build )
          .build

        sch.shape.fieldDescriptions.head.fieldName shouldBe "int_field"
    }

    it should "be able to rebuild nested fields" in {
        case class Core( int : Int, str : String, bool : Boolean )
        case class InnerClass( core : Core )
        case class OuterClass( inner : InnerClass )

        val sch = SchemaBuilder[ OuterClass ]
          .caseClass
          .updateField( "inner" )(
              _.fieldName( "inner_field" )
                .rebuildSchema(
                    _.updateField( "core" )(
                        _.fieldName( "core_field" )
                          .rebuildSchema(
                              _.updateField( "str" )(
                                  _.fieldName( "string_field" ).build
                              ).build
                          ).build
                    ).build
                ).build
          )
          .build

        val outerFd = sch.shape.fieldDescriptions
        val innerFd = outerFd.head.schema.shape.fieldDescriptions
        val coreFd = innerFd.head.schema.shape.fieldDescriptions

        outerFd.size shouldBe 1
        outerFd.head.fieldName shouldBe "inner_field"
        innerFd.size shouldBe 1
        innerFd.head.fieldName shouldBe "core_field"
        coreFd.size shouldBe 3
        coreFd.head.fieldName shouldBe "int"
        coreFd.tail.head.fieldName shouldBe "string_field"
        coreFd.tail.tail.head.fieldName shouldBe "bool"
    }

}
