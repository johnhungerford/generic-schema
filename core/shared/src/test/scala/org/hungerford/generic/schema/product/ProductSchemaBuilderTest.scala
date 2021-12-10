package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.FieldBuilder
import org.hungerford.generic.schema.types.Provider
import org.hungerford.generic.schema.{Schema, Primitive, SchemaProvider}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.hungerford.generic.schema.selector.Selector

import org.hungerford.generic.schema.Default.dsl.*

class ProductSchemaBuilderTest extends AnyFlatSpecLike with Matchers {

    behavior of "ProductSchemaBuilder"

    case class TestCase( int : Int, str : String )

    it should "be able to add fields" in {
        Schema.productBuilder[ Int ]
          .addField(
              FieldBuilder[ Int ].fromSchema( Schema.primitive )
                .fieldName( "some name" )
                .build
          )
          .addField(
              FieldBuilder[ String ].fromSchema( Schema.primitive )
                .fieldName( "some other name" )
                .build
          )
          .addField(
              FieldBuilder[ Boolean ].fromSchema( Schema.primitive )
                .description( "test description" )
                .fieldName( "bool" )
                .build
          )
          .addField(
              FieldBuilder[ BigInt ].fromSchema( Schema.primitive )
                .fieldName( "and another" )
                .build
          )
    }

    it should "be able to add constructor" in {
        Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldBuilder[ String ].primitive.fieldName( "str" ).build )
          .construct( (int, str) => TestCase( int, str ) )
    }

    it should "be able to add deconstructor" in {
        Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldBuilder[ String ].primitive.fieldName( "str" ).build )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
    }

    it should "be able to build schema if constructor and deconstructor are provided" in {
        Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldBuilder[ String ].primitive.fieldName( "str" ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
          .build
    }

    it should "be able to remove field descriptions" in {
        Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldBuilder[ String ].primitive.fieldName( "str" ).build )
          .addField( FieldBuilder[ Boolean ].primitive.fieldName( "bool" ).build )
          .removeField( "bool" )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
          .build

        Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ String ].primitive.fieldName( "str" ).build )
          .addField( FieldBuilder[ Boolean ].primitive.fieldName( "bool" ).build )
          .addField( FieldBuilder[ Int ].primitive.fieldName( "int" ).build )
          .removeField( "bool" )
          .removeField( "str" )
          .addField( FieldBuilder[ String ].primitive.fieldName( "str" ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
          .build
    }

    it should "be able to rebuild" in {
        val schema = Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ Int ].primitive.fieldName( "int" ).build )
          .addField( FieldBuilder[ String ].primitive.fieldName( "str" ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
          .build

        schema.rebuild
          .removeField( "str" )
          .addField( FieldBuilder[ String ].primitive.fieldName( "string" ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( ( value : TestCase ) => (value.int, value.str) )
          .build

    }

    it should "be able to add fields after adding constructor/deconstructor, but require that you rebuild the constructor/deconstructor" in {
      val builder = Schema.productBuilder[ TestCase ]
        .addField( FieldBuilder[ Int ].primitive.fieldName( "int" ).build )
        .addField( FieldBuilder[ String ].primitive.fieldName( "str" ).build )
        .construct( (int, str) => TestCase( int, str ) )
        .deconstruct( v => (v.int, v.str) )
      
      assertCompiles( """builder.build""" )

      val newBuilder = builder.addField( FieldBuilder[ Boolean ].primitive.fieldName( "bool" ).build )

      assertDoesNotCompile( """newBuilder.build""")

      newBuilder
        .construct( (int, str, _) => TestCase( int, str ) )
        .deconstruct( v => (v.int, v.str, false) )
        .build
    }

    it should "preserve constructor/deconstructor when updating additional fields to same type, but does not preserve when additional fields type is changed" in {
        case class TC( int : Int, fields : Map[ String, String ] )

        val builder = Schema.productBuilder[ TC ]
          .addField( FieldBuilder[ Int ].primitive.fieldName( "int" ).build )
          .additionalFields[ String ].fromSchema( Schema.primitive )
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
        val sch = Schema.derivedBuilder[ TestCase ]
          .rebuildField( "int" )( _.fieldName( "int_field" ).build )
          .build

        sch.shape.fieldDescriptions.head.fieldName shouldBe "int_field"
    }

    it should "be able to rebuild nested fields" in {
        case class Core( int : Int, str : String, bool : Boolean )
        case class InnerClass( core : Core )
        case class OuterClass( inner : InnerClass )

        val sch = Schema.derivedBuilder[ OuterClass ]
          .rebuildField( "inner" )(
              _.fieldName( "inner_field" )
                .rebuildSchema(
                    _.rebuildField( "core" )(
                        _.fieldName( "core_field" )
                          .rebuildSchema(
                              _.rebuildField( "str" )(
                                  _.fieldName( "string_field" ).build
                              ).build
                          ).build
                    ).build
                ).build
          ).build

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

    it should "be able to update nested fields using a selector" in {
        case class Core( int : Int, str : String, bool : Boolean )
        case class InnerClass( core : Core )
        case class OuterClass( inner : InnerClass )

        import org.hungerford.generic.schema.selector.Selector.*

        val sch = Schema.derivedBuilder[ OuterClass ]
          .modifyComponent( "inner" / "core" / "str" )(
              v => FieldBuilder.from( v ).fieldName( "string_field" ).build,
          )
          .build

        val outerFd = sch.shape.fieldDescriptions
        val innerFd = outerFd.head.schema.shape.fieldDescriptions
        val coreFd = innerFd.head.schema.shape.fieldDescriptions

        coreFd.tail.head.fieldName shouldBe "string_field"
    }

    it should "be able to build nested products using givens" in {
        case class Core( int : Int, str : String, bool : Boolean )
        case class Inner( core : Core )
        case class Outer( inner : Inner )

        val coreSch = Schema.derivedBuilder[ Core ]
          .rebuildField( "str" )( _.fieldName( "string_field" ).build )
          .build
        import coreSch.givenSchema

        val innerSch = Schema.productBuilder[ Inner ]
          .addField( FieldBuilder[ Core ].fieldName( "core_field" ).fromSchema.build )
          .construct( Inner.apply )
          .deconstruct( v => v.core )
          .build
        import innerSch.givenSchema

        val outerSch = Schema.productBuilder[ Outer ]
          .addField( FieldBuilder[ Inner ].fieldName( "inner" ).fromSchema.build )
          .construct( Outer.apply )
          .deconstruct( v => v.inner )
          .build

        outerSch.shape.fieldDescriptions.head.schema.shape.fieldDescriptions.head.schema.shape.fieldDescriptions.tail.head.fieldName shouldBe "string_field"
    }

}
