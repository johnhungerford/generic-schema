package org.hungerford.generic.schema.selector

import org.scalatest.flatspec.AnyFlatSpecLike

class SelectorTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "Selector building"

    it should "build a selector using fields" in {
        val something = Selector.field( "one" ) /- "two" /- "three"

        assertCompiles( """summon[ something.type <:< Selector[ FieldSelector[ "one" ] *: FieldSelector[ "two" ] *: FieldSelector[ "three" ] *: EmptyTuple ] ]""" )
    }

    it should "build a selector using subtypes" in {
        val something = Selector.subtype( "one" ) /~ "two" /~ "three"

        assertCompiles( """summon[ something.type <:< Selector[ SubTypeSelector[ "one" ] *: SubTypeSelector[ "two" ] *: SubTypeSelector[ "three" ] *: EmptyTuple ] ]""" )
    }

    it should "build a selector using both fields and subtypes" in {
        val something = Selector.field( "field one" ) /~ "subtype one" /- "field two" /~ "subtype two" /~ "subtype three" /- "field three"

        assertCompiles( """summon[ something.type <:< Selector[ FieldSelector[ "field one" ] *: SubTypeSelector[ "subtype one" ] *: FieldSelector[ "field two" ] *: SubTypeSelector[ "subtype two" ] *: SubTypeSelector[ "subtype three" ] *: FieldSelector[ "field three" ] *: EmptyTuple ] ]""" )
    }

    it should "build a selector from a string using AmbigSelector" in {
        import Selector.*

        val something = "ambiguous" /~ "subtype" /- "field"

        assertCompiles( """summon[ something.type <:< Selector[ AmbigSelector[ "ambiguous" ] *: SubTypeSelector[ "subtype" ] *: FieldSelector[ "field" ] *: EmptyTuple ] ]""" )
    }

}
