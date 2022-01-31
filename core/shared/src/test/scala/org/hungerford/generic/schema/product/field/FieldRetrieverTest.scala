package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Default.dsl.*


class FieldRetrieverTest extends AnyFlatSpecLike with Matchers {

    case class TestClass( int : Int, str : String, bool : Boolean )

    val fields = Schema.derived[ TestClass ]
      .shape
      .fieldDescriptions

    behavior of "FieldRetriever using field name"

    it should "retrieve a field description from a field description tuple using field name" in {
        val intField = FieldRetriever.retrieve( "int", fields )
        val strField = FieldRetriever.retrieve( "str", fields )
        val boolField = FieldRetriever.retrieve( "bool", fields )

        intField.description.isEmpty shouldBe true
        intField.fieldName shouldBe "int"
        strField.description.isEmpty shouldBe true
        strField.fieldName shouldBe "str"
        boolField.description.isEmpty shouldBe true
        boolField.fieldName shouldBe "bool"
    }

    it should "not be able to retrieve a field using a non-existent field name" in {
        assertDoesNotCompile( """FieldRetriever.retrieve( "non-existent-field", fields )""" )
    }

    behavior of "FieldRetriever using index"

    it should "retrieve a field description from a field description tuple using field name" in {
        val intField = FieldRetriever.retrieve( 0, fields )
        val strField = FieldRetriever.retrieve( 1, fields )
        val boolField = FieldRetriever.retrieve( 2, fields )

        intField.description.isEmpty shouldBe true
        intField.fieldName shouldBe "int"
        strField.description.isEmpty shouldBe true
        strField.fieldName shouldBe "str"
        boolField.description.isEmpty shouldBe true
        boolField.fieldName shouldBe "bool"
    }

    it should "not be able to retrieve a field using an out-of-bound index" in {
        assertDoesNotCompile( """FieldRetriever.retrieve( 3, fields )""" )
    }

}
