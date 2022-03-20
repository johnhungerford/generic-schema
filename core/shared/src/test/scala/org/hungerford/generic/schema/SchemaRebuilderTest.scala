package org.hungerford.generic.schema

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.Default.dsl.*
import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.validator.Validator

import scala.util.Try

class SchemaRebuilderTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {
    behavior of "SchemaRebuilder"

    it should "rebuild a product schema" in {
        case class Prod( int : Int, str : String )

        val sch = Schema.derivedBuilder[ Prod ]
          .removeField( "str" )
          .construct( i => Prod( i, i.toString ) )
          .build

        sch.shape.fieldDescriptions.size shouldBe 1

        val sch2 = sch.rebuild
          .addField( Field.builder[ Prod, String ].name( "str" ).primitive.extractor( _.str ).build )
          .construct( (i, s) => Prod( i, s ) )
          .build

        sch2.shape.fieldDescriptions.size shouldBe 2
    }

//    it should "rebuild a coproduct schema" in {
//        case class Possible( number : String )
//
//        val sch = Schema.coproductBuilder[ Possible ]
//          .buildSubtype[ Int ]( _.typeName( "PosInt" ).primitive.validate( Validator.positiveOrZero, Validator.nonZero ).toSuper( v => Possible( v.toString ) ).fromSuper( v => Try( v.number.toInt ).toOption ).build )
//          .buildSubtype[ Double ]( _.typeName( "NegDbl" ).primitive.validate( Validator.negativeOrZero, Validator.nonZero ).toSuper( v => Possible( v.toString ) ).fromSuper( v => Try( v.number.toDouble ).toOption ).build )
//          .build
//
//        val sch2 = sch.rebuild
//          .buildSubtype[ Float ]( _.typeName( "ZeroFloat" ).primitive.validate( Validator.oneOf( 0F ) ).toSuper( v => Possible( v.toString ) ).fromSuper( v => Try( v.number.toFloat ).toOption ).build )
//          .description( "A numeric type supporting integers for positive numbers, doubles for negative numbers, and 0 as float" )
//          .build
//    }
}
