package org.hungerford.generic.schema.types

import org.scalatest.flatspec.AnyFlatSpecLike

class ContainsTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "Contains"

    it should "not provide an instance for empty tuple" in {
        assertDoesNotCompile( """summon[Contains[EmptyTuple, Int]]""")
    }

    it should "not provide an instance for 1-tuple of a different type" in {
        assertDoesNotCompile( """summon[Contains[String *: EmptyTuple, Int]]""")
    }

    it should "not provide an instance for N-tuple that does not contain the type" in {
        assertDoesNotCompile( """summon[Contains[String *: Int *: Boolean *: Map[Int, String] *: EmptyTuple, Double]]""")
    }

    it should "provide an instance for a 1-tuple of the same type" in {
        assertCompiles( """summon[Contains[String *: EmptyTuple, String]]""")
    }

    it should "provide an instance for an N-tuple that contains the same type" in {
        assertCompiles( """summon[Contains[String *: Int *: Boolean *: Map[Int, String] *: Double *: EmptyTuple, String]]""")
        assertCompiles( """summon[Contains[String *: Int *: Boolean *: Map[Int, String] *: Double *: EmptyTuple, Int]]""")
        assertCompiles( """summon[Contains[String *: Int *: Boolean *: Map[Int, String] *: Double *: EmptyTuple, Boolean]]""")
        assertCompiles( """summon[Contains[String *: Int *: Boolean *: Map[Int, String] *: Double *: EmptyTuple, Map[Int, String]]]""")
        assertCompiles( """summon[Contains[String *: Int *: Boolean *: Map[Int, String] *: Double *: EmptyTuple, Double]]""")
    }
}
