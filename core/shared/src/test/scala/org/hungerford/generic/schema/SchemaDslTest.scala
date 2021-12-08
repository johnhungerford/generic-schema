package org.hungerford.generic.schema

import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.product.field.{FieldDsl, Field}

class SchemaDslTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "static schema dsl"

    case class TestCase( int : Int, str : String )
    object TestSchemaDsl extends SchemaDsl with FieldDsl

    it should "allow building schemas" in {
        assertDoesNotCompile( """Schema.primitiveBuilder[ Int ].description( "test-description" ).build""" )
        assertDoesNotCompile( """Schema.productBuilder[ TestCase ]
                                |          .addField( Field.builder[ Int ].fieldName( "int" ).primitive.build )
                                |          .addField( Field.builder[ String ].fieldName( "str" ).primitive.build )
                                |          .construct( (int, str) => TestCase( int, str ) )
                                |          .deconstruct( v => (v.int, v.str) )
                                |          .build""".stripMargin )
        assertDoesNotCompile( """Schema.derivedBuilder[ TestCase ].build""" )
        assertDoesNotCompile( """Schema.derived[ TestCase ]""" )

        import TestSchemaDsl.*

        Schema.primitiveBuilder[ Int ].description( "test-description" ).build
        Schema.productBuilder[ TestCase ]
          .addField( Field.builder[ Int ].fieldName( "int" ).primitive.build )
          .addField( Field.builder[ String ].fieldName( "str" ).primitive.build )
          .construct( (int, str) => TestCase( int, str ) )
          .deconstruct( v => (v.int, v.str) )
          .build
        Schema.derivedBuilder[ TestCase ].build
        Schema.derived[ TestCase ]
    }

    it should "allow modifying schemas" in {

        val sch = {
            import TestSchemaDsl.*
            Schema.derived[ TestCase ]
        }

        assertDoesNotCompile( """sch.withDescription( "test-description" )""" )
        assertDoesNotCompile( """sch.withDescription( "test-description" ).withoutDescription""" )
        assertDoesNotCompile( """sch.modifyComponent( "str" )( _.withName( "string_field" ) )""" )
        assertDoesNotCompile( """sch.rebuild.removeField( "int" )
                                |          .construct( str => TestCase( 5, str ) )
                                |          .deconstruct( _.str )
                                |          .build""".stripMargin )

        import TestSchemaDsl.*

        val withDesc = sch.withDescription( "test-description" )
        withDesc.genericDescription shouldBe Some( "test-description" )
        withDesc.withoutDescription.genericDescription shouldBe None

        val updated = sch.modifyComponent( "str" )( _.withName( "string_field" ) )
        updated( "string_field" ).fieldName shouldBe "string_field"

        sch.rebuild.removeField( "int" )
          .construct( str => TestCase( 5, str ) )
          .deconstruct( _.str )
          .build

    }

}
