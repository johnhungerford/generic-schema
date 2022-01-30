package org.hungerford.generic.schema.types

import org.scalatest.flatspec.AnyFlatSpecLike

class NatTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    import Nat.*

    behavior of "Plus"

    it should "add anything to zero" in {
        val plus4 = summon[Plus[_0, _4]]
        summon[plus4.Res =:= _4]

        val plus9 = summon[Plus[_0, _9]]
        summon[plus9.Res =:= _9]
    }

    it should "add to one" in {
        val plus1_1 = summon[Plus[_1, _1]]
        summon[plus1_1.Res =:= _2]

        val plus1_2 = summon[Plus[_1, _2]]
        summon[plus1_2.Res =:= _3]

        val plus2_1 = summon[Plus[_2, _1]]
        summon[plus2_1.Res =:= _3]

        val plus1_9 = summon[Plus[_1, _9]]
        summon[plus1_9.Res =:= _10]

        val plus9_1 = summon[Plus[_9, _1]]
        summon[plus9_1.Res =:= _10]
    }

    it should "add anything to anything" in {
        val plus5_5 = summon[Plus[_5, _5]]
        summon[plus5_5.Res =:= _10]

        val plus3_8 = summon[Plus[_3, _8]]
        summon[plus3_8.Res =:= _11]

        val plus3_7 = summon[Plus[_3, _7]]
        summon[plus3_7.Res =:= _10]

        val plus2_8 = summon[Plus[_2, _8]]
        summon[plus2_8.Res =:= _10]
    }

    behavior of "Minus"

    it should "subtract zero from a nat, giving the nat" in {
        val minus5 = summon[Minus[_5, _0]]
        summon[minus5.Res =:= _5]

        val minus0 = summon[Minus[_0, _0]]
        summon[minus0.Res =:= _0]

        val minus1 = summon[Minus[_1, _0]]
        summon[minus1.Res =:= _1]

        val minus10 = summon[Minus[_10, _0]]
        summon[minus10.Res =:= _10]
    }

    it should "subtract one from a nat, given the correct result" in {
        val minus5 = summon[Minus[_5, _1]]
        summon[minus5.Res =:= _4]

        val minus1 = summon[Minus[_1, _1]]
        summon[minus1.Res =:= _0]

        val minus10 = summon[Minus[_10, _1]]
        summon[minus10.Res =:= _9]
    }

    it should "subtract two from a nat, given the correct result" in {
        val minus5 = summon[Minus[_5, _2]]
        summon[minus5.Res =:= _3]

        val minus2 = summon[Minus[_2, _2]]
        summon[minus2.Res =:= _0]

        val minus10 = summon[Minus[_10, _2]]
        summon[minus10.Res =:= _8]
    }

    it should "subtract anything, given the correct result" in {
        val minus5 = summon[Minus[_9, _8]]
        summon[minus5.Res =:= _1]

        val minus2 = summon[Minus[_11, _5]]
        summon[minus2.Res =:= _6]

        val minus10 = summon[Minus[_6, _6]]
        summon[minus10.Res =:= _0]
    }
}
