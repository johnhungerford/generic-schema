package org.hungerford.generic.schema.types

import org.scalatest.flatspec.AnyFlatSpecLike

class ExistsForTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "ExistsFor"

    it should "be summonable for a type class that exists for the given type" in {
        summon[ ExistsFor[ Numeric, Int ] ]
    }

    it should "not be summonable for a type class that does not exist for the given type" in {
        assertDoesNotCompile( """summon[ ExistsFor[ Numeric, String ] ]""" )
    }

    it should "be summonable for a type class that has no instance for empty tuple" in {
        summon[ ExistsFor[ Numeric, EmptyTuple ] ]
    }

    it should "be summonable for a type class that exists for the single element of a 1-tuple" in {
        summon[ ExistsFor[ Numeric, Tuple1[ Int ] ] ]
    }

    it should "be summonable for a type class that exists for every type in an n-tuple" in {
        summon[ ExistsFor[ Numeric, (Int, Float) ] ]
        summon[ ExistsFor[ Numeric, (Int, Float, Double) ] ]
        summon[ ExistsFor[ Numeric, (Int, Float, Double, BigInt) ] ]
        summon[ ExistsFor[ Numeric, (Int, Float, Double, BigInt, Long) ] ]
    }

    it should "not be summonable if even one type in an n-tuple is does not have the type class instance" in {
        assertDoesNotCompile( """summon[ ExistsFor[ Numeric, (Int, String) ] ]""" )
        assertDoesNotCompile( """summon[ ExistsFor[ Numeric, (Map[String, String], Float, Double) ] ]""" )
        assertDoesNotCompile( """summon[ ExistsFor[ Numeric, (Int, Float, Boolean, BigInt) ] ]""" )
        assertDoesNotCompile( """summon[ ExistsFor[ Numeric, (Int, Float, Double, Unit, Long) ] ]""" )
    }
}
