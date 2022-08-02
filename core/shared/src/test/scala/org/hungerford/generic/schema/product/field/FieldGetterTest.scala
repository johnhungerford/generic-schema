package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.Schema
import generic.schema.exports.*

class FieldGetterTest extends AnyFlatSpecLike with Matchers {

    behavior of "FieldGetter"

    case class TestCase( int : Int, str : String, bool : Boolean )

    it should "extract a field from a tuple" in {
        val sch = Schema.derived[ TestCase ]

        val tc = TestCase( 5, "hello", true )

        val tcTuple = sch.shape.deconstruct( tc )

        tcTuple shouldBe ( 5, "hello", true )

        FieldGetter.get( tcTuple, "int", sch.shape.fieldDescriptions ) shouldBe 5
        FieldGetter.get( tcTuple, "str", sch.shape.fieldDescriptions ) shouldBe "hello"
        FieldGetter.get( tcTuple, "bool", sch.shape.fieldDescriptions ) shouldBe true
    }

}
