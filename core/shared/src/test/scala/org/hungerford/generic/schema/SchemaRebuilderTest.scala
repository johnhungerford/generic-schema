package org.hungerford.generic.schema

import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.Default.dsl.*
import org.hungerford.generic.schema.product.field.Field

class SchemaRebuilderTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {
    behavior of "SchemaRebuilder"

    it should "rebuild a coproduct schema" in {
        trait SuperType
        case object SubType extends SuperType
        case object SubType2 extends SuperType

        val sch = Schema.coproductBuilder[ SuperType ]
          .buildSubtype[ SubType.type ]( _.typeName( "sub-type-1" ).primitive.build )
          .build

        sch.shape.subtypeDescriptions.size shouldBe 1

        val sch2 = sch.rebuild
          .buildSubtype[ SubType2.type ]( _.typeName( "sub-type-2" ).primitive.build )
          .build

        sch2.shape.subtypeDescriptions.size shouldBe 2
    }

    it should "rebuild a product schema" in {
        case class Prod( int : Int, str : String )

        val sch = Schema.derivedBuilder[ Prod ]
          .removeField( "str" )
          .construct( i => Prod( i, i.toString ) )
          .deconstruct( _.int )
          .build

        sch.shape.fieldDescriptions.size shouldBe 1

        val sch2 = sch.rebuild
          .addField( Field.builder[ String ].fieldName( "str" ).primitive.build )
          .construct( (i, s) => Prod( i, s ) )
          .deconstruct( p => (p.int, p.str) )
          .build

        sch2.shape.fieldDescriptions.size shouldBe 2
    }
}
