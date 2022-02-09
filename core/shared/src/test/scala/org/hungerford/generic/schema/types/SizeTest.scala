package org.hungerford.generic.schema.types

import org.scalatest.flatspec.AnyFlatSpecLike

class SizeTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "Size"

    it should "give size of tuples" in {
        val empty = summon[Size[EmptyTuple]]
        empty.size shouldBe 0

        val one = summon[Size[Int *: EmptyTuple]]
        one.size shouldBe 1

        val two = summon[Size[Int *: Boolean *: EmptyTuple]]
        two.size shouldBe 2

        val ten = summon[Size[(Int, Boolean, Float, String, Double, List[Int], Map[String, String], BigInt, Char, Long)]]
        ten.size shouldBe 10
    }

}
