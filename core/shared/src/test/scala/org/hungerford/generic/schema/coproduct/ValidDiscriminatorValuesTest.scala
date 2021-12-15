package org.hungerford.generic.schema.coproduct

import org.scalatest.flatspec.AnyFlatSpecLike

class ValidDiscriminatorValuesTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "ValidDiscriminatorValues"

    it should "exist for any discriminator type and an empty tuple" in {
        assertCompiles( """summon[ ValidDiscriminatorValues[ Map[ Float, BigInt ], EmptyTuple ] ]""" )
        assertCompiles( """summon[ ValidDiscriminatorValues[ Array[ Boolean ], EmptyTuple ] ]""" )
    }

    it should "exist for string and a tuple of string singletons" in {
        assertCompiles( """summon[ ValidDiscriminatorValues[ String, "a" *: "b" *: "c" *: EmptyTuple ] ]""" )
        assertDoesNotCompile( """summon[ ValidDiscriminatorValues[ String, "a" *: 2 *: "c" *: EmptyTuple ] ]""" )
    }

}
