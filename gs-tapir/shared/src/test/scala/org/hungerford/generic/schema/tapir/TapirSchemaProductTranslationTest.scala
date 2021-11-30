package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.SchemaBuilder
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import sttp.tapir.Schema

class TapirSchemaProductTranslationTest
  extends AnyFlatSpecLike
    with Matchers
    with TapirSchemaProductTranslation {

    behavior of "TapirSchemaProductTranslation"

    case class TestCase( int : Int, str : String, bool : Boolean )

    it should "translate a product schema" in {
        val sch = SchemaBuilder[ TestCase ].caseClass.build

        assertCompiles( """val tapirSchema : Schema[ TestCase ] = SchemaTranslator.translate( sch )""" )
    }

}
