package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.translation.SchemaTranslator
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.Default.dsl.*

import sttp.tapir
import sttp.tapir.{Schema => TapirSchema}
import sttp.tapir.SchemaType.{SProductField, SProduct}

class TapirSchemaProductTranslationTest
  extends AnyFlatSpecLike
    with Matchers
    with TapirSchemaProductTranslation {

    behavior of "TapirSchemaProductTranslation"

    case class TestCase( int : Int, str : String, bool : Boolean )

    it should "translate a product schema into an SProduct schema type with the correct field names, correct getters, correct validators, and the correct description" in {
        val sch = Schema.derivedBuilder[ TestCase ]
          .description( "test-case-description" )
          .build
          .modifyComponent( "int" )( _.addValidators( Validator.min[ Int ]( -5 ), Validator.maxExclusive[ Int ]( 25 ), Validator.nonZero[ Int ] ) )

        val tapirSchema : TapirSchema[ TestCase ] = SchemaTranslator.translate( sch )

        val tc = TestCase( 5, "hello", true )

        tapirSchema.schemaType match {
            case SProduct( fields : List[ SProductField[ TestCase ] ] ) =>
                fields.size shouldBe 3
                fields.map( _.name.name ).toSet shouldBe Set( "int", "str", "bool" )
                fields.find( _.name.name == "int" ).get.get( tc ) shouldBe Some( 5 )
                fields.find( _.name.name == "str" ).get.get( tc ) shouldBe Some( "hello" )
                fields.find( _.name.name == "bool" ).get.get( tc ) shouldBe Some( true )
                val intValidator : tapir.Validator[ Int ] =
                    fields.find( _.name.name == "int" ).get.schema.validator.asInstanceOf[ tapir.Validator[ Int ] ]
                intValidator( -6 ).isEmpty shouldBe false
                intValidator( -5 ).isEmpty shouldBe true
                intValidator( -1 ).isEmpty shouldBe true
                intValidator( 0 ).isEmpty shouldBe false
                intValidator( 1 ).isEmpty shouldBe true
                intValidator( 10 ).isEmpty shouldBe true
                intValidator( 24 ).isEmpty shouldBe true
                intValidator( 25 ).isEmpty shouldBe false
                intValidator( 26 ).isEmpty shouldBe false

            case _ => fail( "Product schema did not generate an SProduct tapir schema type" )
        }

        tapirSchema.description should contain( "test-case-description" )
    }

}
