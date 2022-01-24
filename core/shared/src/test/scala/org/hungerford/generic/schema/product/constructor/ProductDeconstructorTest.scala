package org.hungerford.generic.schema.product.constructor

import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.product.field.Field

class ProductDeconstructorTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    it should "provide an instance for an empty tuple" in {
        assertCompiles("""summon[ProductDeconstructor[Int, EmptyTuple]]""")
    }

    it should "provide an instance for unit and an empty tuple" in {
        assertCompiles( """summon[ProductDeconstructor[Int, (Unit, EmptyTuple)]]""" )
    }

    it should "provide an instance for unit and a non-empty tuple of fields that agree with parent type" in {
        assertCompiles( """summon[ProductDeconstructor[Int, (Unit, (Field[Int, String], Field[Int, Boolean]))]]""" )
        assertCompiles( """summon[ProductDeconstructor[Int, (Unit, (Field.Aux[Int, String, "str", Unit], Field.Aux[Int, Boolean, "bool", Unit]))]]""" )
        assertCompiles( """summon[ProductDeconstructor[Int, (Unit, (Field.Aux[Int, Int, "int", Unit], Field.Aux[Int, String, "str", Unit], Field.Aux[Int, Boolean, "bool", Unit]))]]""" )
    }
    
}
