package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.SchemaBuilder
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import sttp.tapir.Schema
import sttp.tapir.SchemaType.{SProductField, SProduct}

class TapirSchemaProductTranslationTest
  extends AnyFlatSpecLike
    with Matchers
    with TapirSchemaProductTranslation {

    behavior of "TapirSchemaProductTranslation"

    case class TestCase( int : Int, str : String, bool : Boolean )

    it should "translate a product schema into an SProduct schema type with the correct field names and the correct description" in {
        val sch = SchemaBuilder[ TestCase ].caseClass.description( "test-case-description" ).build

        val tapirSchema : Schema[ TestCase ] = SchemaTranslator.translate( sch )

        tapirSchema.schemaType match {
            case SProduct( fields : List[ SProductField[ TestCase ] ] ) =>
                fields.size shouldBe 3
                fields.map( _.name.name ).toSet shouldBe Set( "int", "str", "bool" )
            case _ => fail( "Product schema did not generate an SProduct tapir schema type" )
        }

        tapirSchema.description should contain( "test-case-description" )
    }

}
