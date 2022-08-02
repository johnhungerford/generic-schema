package org.hungerford.generic.schema.utilities

import org.scalatest.flatspec.AnyFlatSpecLike


class LensDslTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    val sch = {
        import generic.schema.exports.{*, given}

        Schema.derived[TestNestedProd]
    }

    val nc2Sch = {
        import generic.schema.exports.{*, given}

        Schema.derived[NestedCase2]
    }

    behavior of "LensDsl"

    import org.hungerford.generic.schema.utilities.LensDsl.{*, given}

    val testVal1 = TestNestedProd(NestedCase2(50))
    val testVal2 = TestNestedProd(NestedCase1)

    import sch.givenSchema

    it should "support modifying a nested value" in {
        val newVal = testVal1.select("a" / "NestedCase2" / "b" ).modify(_ * 2)
        newVal shouldBe TestNestedProd(NestedCase2(100))
        val newVal2 = testVal2.select("a" / "NestedCase2" / "b").modify(_ * 2)
        newVal2 shouldBe newVal2
    }

    it should "support retrieving a nested value" in {
        val newVal = testVal1.select("a" / "NestedCase2" / "b" ).retrieve
        newVal shouldBe Some( 50 )
        val newVal2 = testVal2.select("a" / "NestedCase2" / "b").retrieve
        newVal2 shouldBe None
    }

}
