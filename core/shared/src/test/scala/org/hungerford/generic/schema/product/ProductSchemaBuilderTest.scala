package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.FieldBuilder
import org.hungerford.generic.schema.types.Provider
import org.hungerford.generic.schema.{Schema, Primitive, SchemaProvider}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.hungerford.generic.schema.selector.Selector

import generic.schema.exports.*

class ProductSchemaBuilderTest extends AnyFlatSpecLike with Matchers {

    behavior of "ProductSchemaBuilder"

    case class TestCase( int : Int, str : String )

    it should "be able to add fields" in {
        Schema.productBuilder[ TestCase ]
          .addField(
              FieldBuilder[ TestCase, Int ]
                .fromSchema( Schema.primitive )
                .name( "some name" )
                .extractor( _.int )
                .build
          )
          .addField(
              FieldBuilder[ TestCase, String ]
                .fromSchema( Schema.primitive )
                .name( "some other name" )
                .extractor( _.str )
                .build
          )
          .addField(
              FieldBuilder[ TestCase, Boolean ]
                .fromSchema( Schema.primitive )
                .description( "test description" )
                .name( "bool" )
                .extractor( _ => true )
                .build
          )
          .addField(
              FieldBuilder[ TestCase, BigInt ]
                .fromSchema( Schema.primitive )
                .name( "and another" )
                .extractor( _ => BigInt( "12345678987654321" ) )
                .build
          )
    }

    it should "be able to build fields" in {
        Schema.productBuilder[ TestCase ]
          .buildField[ Int ](
              _.fromSchema( Schema.primitive )
                .name( "some name" )
                .extractor( _.int )
                .build
              )
          .buildField[ String ](
              _.fromSchema( Schema.primitive )
                .name( "some other name" )
                .extractor( _.str )
                .build
              )
          .buildField[ Boolean ](
              _.fromSchema( Schema.primitive )
                .description( "test description" )
                .name( "bool" )
                .extractor( _ => true )
                .build
              )
          .buildField(
              _.fromSchema( Schema.primitive )
                .name( "and another" )
                .extractor( _ => BigInt( "12345678987654321" ) )
                .build
              )
    }

    it should "be able to add constructor and build" in {
        Schema.productBuilder[ TestCase ]
          .buildField[ Int ]( _.primitive.name( "int" ).extractor( _.int ).build )
          .buildField[ String ]( _.primitive.name( "str" ).extractor( _.str ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .build
    }

    it should "be able to remove field descriptions" in {
        Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ TestCase, Int ].primitive.name( "int" ).extractor( _.int ).build )
          .addField( FieldBuilder[ TestCase, String ].primitive.name( "str" ).extractor( _.str ).build )
          .addField( FieldBuilder[ TestCase, Boolean ].primitive.name( "bool" ).extractor( _ => true ).build )
          .removeField( "bool" )
          .construct( (int, str) => TestCase( int, str ) )
          .build

        Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ TestCase, String ].primitive.name( "str" ).extractor( _.str ).build )
          .addField( FieldBuilder[ TestCase, Boolean ].primitive.name( "bool" ).extractor( _ => true ).build )
          .addField( FieldBuilder[ TestCase, Int ].primitive.name( "int" ).extractor( _.int ).build )
          .removeField( "bool" )
          .removeField( "str" )
          .addField( FieldBuilder[ TestCase, String ].primitive.name( "str" ).extractor( _.str ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .build
    }

    it should "be able to remove field descriptions by type" in {
        Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ TestCase, Int ].primitive.name( "int" ).extractor( _.int ).build )
          .addField( FieldBuilder[ TestCase, String ].primitive.name( "str" ).extractor( _.str ).build )
          .addField( FieldBuilder[ TestCase, Boolean ].primitive.name( "bool" ).extractor( _ => true ).build )
          .removeField( t[ Boolean ] )
          .construct( (int, str) => TestCase( int, str ) )
          .build

        Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ TestCase, String ].primitive.name( "str" ).extractor( _.str ).build )
          .addField( FieldBuilder[ TestCase, Boolean ].primitive.name( "bool" ).extractor( _ => true ).build )
          .addField( FieldBuilder[ TestCase, Int ].primitive.name( "int" ).extractor( _.int ).build )
          .removeField( t[ Boolean ] )
          .removeField( tN[ String ]( 0 ) )
          .addField( FieldBuilder[ TestCase, String ].primitive.name( "str" ).extractor( _.str ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .build

        Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ TestCase, String ].primitive.name( "str" ).extractor( _.str ).build )
          .addField( FieldBuilder[ TestCase, Boolean ].primitive.name( "bool" ).extractor( _ => true ).build )
          .addField( FieldBuilder[ TestCase, Int ].primitive.name( "int" ).extractor( _.int ).build )
          .addField( FieldBuilder[ TestCase, Boolean ].primitive.name( "bool2" ).extractor( _ => false ).build )
          .removeField( tN[ Boolean ]( 1 ) ) // removes bool2
          .removeField( "bool" )            // removes bool
          .construct( (str, int) => TestCase( int, str ) )
          .build
    }

    it should "be able to rebuild" in {
        val schema = Schema.productBuilder[ TestCase ]
          .addField( FieldBuilder[ TestCase, Int ].primitive.name( "int" ).extractor( _.int ).build )
          .addField( FieldBuilder[ TestCase, String ].primitive.name( "str" ).extractor( _.str ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .build

        schema.rebuild
          .removeField( "str" )
          .addField( FieldBuilder[ TestCase, String ].primitive.name( "string" ).extractor( _.str ).build )
          .construct( (int, str) => TestCase( int, str ) )
          .build
    }

    it should "be able to add fields after adding constructor, but require that you rebuild the constructor" in {
      val builder = Schema.productBuilder[ TestCase ]
        .addField( FieldBuilder[ TestCase, Int ].primitive.name( "int" ).extractor( _.int ).build )
        .addField( FieldBuilder[ TestCase, String ].primitive.name( "str" ).extractor( _.str ).build )
        .construct( (int, str) => TestCase( int, str ) )

      assertCompiles( """builder.build""" )

      val newBuilder = builder.addField( FieldBuilder[ TestCase, Boolean ].primitive.name( "bool" ).extractor( _ => true ).build )

      assertDoesNotCompile( """newBuilder.build""")

      newBuilder
        .construct( (int, str, _) => TestCase( int, str ) )
        .build
    }

    it should "preserve constructor when updating additional fields to same type, but does not preserve when additional fields type is changed" in {
        case class TC( int : Int, fields : Map[ String, String ] )

        val builder = Schema.productBuilder[ TC ]
          .addField( FieldBuilder[ TC, Int ].primitive.name( "int" ).extractor( _.int ).build )
          .additionalFields[ String ].fromSchema( _.fields )( Schema.primitive )
          .construct( (int, af) => TC( int, af ) )

        assertCompiles( """builder.build""" )

        import org.hungerford.generic.schema.defaults.DefaultSchemas.given

        val newBuilder = builder
          .additionalFields[ String ].fromSchema( _.fields )

        assertCompiles( """newBuilder.build""" )

        val lastBuilder = builder
          .additionalFields[ Double ].fromSchema( v => v.fields.mapValues( _.toDouble ).toMap )

        assertDoesNotCompile( """lastBuilder.build""")

        lastBuilder
          .construct( (int, af) => TC( int, af.mapValues( _.toString ).toMap ) )
          .build
    }

    it should "be able to rebuild a field, selected by name" in {
        val sch = Schema.derivedBuilder[ TestCase ]
          .rebuildField( "int" )( _.name( "int_field" ).build )
          .build

        sch.shape.fieldDescriptions.head.fieldName shouldBe "int_field"
    }

    it should "be able to rebuild nested fields" in {
        case class Core( int : Int, str : String, bool : Boolean )
        case class InnerClass( core : Core )
        case class OuterClass( inner : InnerClass )

        val sch = Schema.derivedBuilder[ OuterClass ]
          .rebuildField( "inner" )(
              _.name( "inner_field" )
                .rebuildSchema(
                    _.rebuildField( "core" )(
                        _.name( "core_field" )
                          .rebuildSchema(
                              _.rebuildField( "str" )(
                                  _.name( "string_field" ).build
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
              v => FieldBuilder.from( v ).name( "string_field" ).build,
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
          .rebuildField( "str" )( _.name( "string_field" ).build )
          .build
        import coreSch.givenSchema

        val innerSch = Schema.productBuilder[ Inner ]
          .addField( FieldBuilder[ Inner, Core ].name( "core_field" ).extractor( _.core ).fromSchema.build )
          .construct( Inner.apply )
          .build
        import innerSch.givenSchema

        val outerSch = Schema.productBuilder[ Outer ]
          .addField( FieldBuilder[ Outer, Inner ].extractor( _.inner ).name( "inner" ).fromSchema.build )
          .construct( Outer.apply )
          .build

        outerSch.shape.fieldDescriptions.head.schema.shape.fieldDescriptions.head.schema.shape.fieldDescriptions.tail.head.fieldName shouldBe "string_field"
    }

}
