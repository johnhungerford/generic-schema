package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.selector.{FieldSelector, SubTypeSelector}
import org.scalatest.flatspec.AnyFlatSpecLike

import org.hungerford.generic.schema.types.Nat

sealed trait TestCoprod
case object Case1 extends TestCoprod
case class Case2(value: Int) extends TestCoprod

class LensTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    import org.hungerford.generic.schema.Default.dsl.{*, given}

    behavior of "Product lens"

    case class TestProd( a: Int, b: String, c: Boolean)
    val prodSch = Schema.derived[TestProd]

    it should "retrieve a product field value successfully" in {
        type TCS = prodSch.Shape

        val lens = summon[Lens[TestProd, TCS, FieldSelector["a"]]]

        val retrievedValue = lens.retrieve( TestProd( 2, "hello", false ), prodSch )
        retrievedValue shouldBe 2

        val lens2 = summon[Lens[TestProd, TCS, FieldSelector[1]]]

        val retrievedValue2 = lens2.retrieve( TestProd( 2, "hello", false ), prodSch )
        retrievedValue2 shouldBe "hello"

        val lens3 = summon[Lens[TestProd, TCS, FieldSelector["c"]]]

        val retrievedValue3 = lens3.retrieve( TestProd( 2, "hello", false ), prodSch )
        retrievedValue3 shouldBe false
    }

    it should "modify a product field value successfully" in {
        type TCS = prodSch.Shape

        val lens = summon[Lens[TestProd, TCS, FieldSelector["a"]]]

        val modifiedValue = lens.modify( TestProd( 2, "hello", false ), prodSch, _ + 5 )
        modifiedValue shouldBe TestProd(7, "hello", false)

        val lens2 = summon[Lens[TestProd, TCS, FieldSelector["b"]]]

        val modifiedValue2 = lens2.modify( TestProd( 2, "hello", false ), prodSch, _ + " world" )
        modifiedValue2 shouldBe TestProd(2, "hello world", false)

        val lens3 = summon[Lens[TestProd, TCS, FieldSelector["c"]]]

        val modifiedValue3 = lens3.modify( TestProd( 2, "hello", false ), prodSch, !_ )
        modifiedValue3 shouldBe TestProd( 2, "hello", true )
    }

    behavior of "CoproductLens"

    val coprSch = Schema.derived[TestCoprod]

    it should "retrieve a coproduct subtype value as Some[T] when the coproduct is that subtype" in {
        type TCS = coprSch.Shape

        val lens = summon[Lens[TestCoprod, TCS, SubTypeSelector["Case1"]]]

        val retrievedValue = lens.retrieve( Case1, coprSch )
        retrievedValue shouldBe Some(Case1)

        val lens2 = summon[Lens[TestCoprod, TCS, SubTypeSelector["Case2"]]]

        val retrievedValue2 = lens2.retrieve( Case2(3), coprSch )
        retrievedValue2 shouldBe Some(Case2(3))
    }

    it should "retrieve a coproduct subtype value as None when the coproduct is not that subtype" in {
        type TCS = coprSch.Shape

        val lens = summon[Lens[TestCoprod, TCS, SubTypeSelector["Case1"]]]

        val retrievedValue = lens.retrieve( Case2(3), coprSch )
        retrievedValue shouldBe None

        val lens2 = summon[Lens[TestCoprod, TCS, SubTypeSelector["Case2"]]]

        val retrievedValue2 = lens2.retrieve( Case1, coprSch )
        retrievedValue2 shouldBe None
    }

    it should "modify a coproduct when the coproduct is the selected subtype" in {
        type TCS = coprSch.Shape

        val lens = summon[Lens[TestCoprod, TCS, SubTypeSelector["Case2"]]]

        val modifiedValue = lens.modify( Case2(3), coprSch, _.copy(value = 10) )
        modifiedValue shouldBe Case2(10)

        val lens2 = summon[Lens[TestCoprod, TCS, SubTypeSelector["Case2"]]]

        val modifiedValue2 = lens2.modify( Case2(3), coprSch, v => v.copy(value = v.value * 20) )
        modifiedValue2 shouldBe Case2(60)
    }

    it should "not modify a coproduct when the coproduct is not the selected subtype" in {
        type TCS = coprSch.Shape

        val lens = summon[Lens[TestCoprod, TCS, SubTypeSelector["Case2"]]]

        val modifiedValue = lens.modify( Case1, coprSch, _.copy(value = 10) )
        modifiedValue shouldBe Case1

        val lens2 = summon[Lens[TestCoprod, TCS, SubTypeSelector["Case2"]]]

        val modifiedValue2 = lens2.modify( Case1, coprSch, v => v.copy(value = v.value * 20) )
        modifiedValue2 shouldBe Case1
    }

}
