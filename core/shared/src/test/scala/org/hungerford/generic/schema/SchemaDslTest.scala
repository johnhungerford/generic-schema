package org.hungerford.generic.schema

import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.product.field.{FieldDsl, Field}

class SchemaDslTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "static schema dsl"

    case class TestCase( int : Int, str : String )
    object TestSchemaDsl extends SchemaDsl with FieldDsl

    it should "allow building schemas" in {
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

}
