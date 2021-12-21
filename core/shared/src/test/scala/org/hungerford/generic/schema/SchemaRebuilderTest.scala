package org.hungerford.generic.schema

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.Default.dsl.*
import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.validator.Validator

class SchemaRebuilderTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {
    behavior of "SchemaRebuilder"

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

    it should "rebuild a coproduct schema" in {
        case class Possible( number : String )

        val sch = Schema.coproductBuilder[ Possible ]
          .buildSubtype[ Int ]( _.typeName( "PosInt" ).primitive.validate( Validator.positiveOrZero, Validator.nonZero ).asSuper( v => Possible( v.toString ) ).build )
          .buildSubtype[ Double ]( _.typeName( "NegDbl" ).primitive.validate( Validator.negativeOrZero, Validator.nonZero ).asSuper( v => Possible( v.toString ) ).build )
          .build

        val sch2 = sch.rebuild
          .buildSubtype[ Float ]( _.typeName( "ZeroFloat" ).primitive.validate( Validator.oneOf( 0F ) ).asSuper( v => Possible( v.toString ) ).build )
          .description( "A numeric type supporting integers for positive numbers, doubles for negative numbers, and 0 as float" )
          .build
    }
}
