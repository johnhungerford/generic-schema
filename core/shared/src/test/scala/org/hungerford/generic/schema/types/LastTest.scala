package org.hungerford.generic.schema.types

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class LastTest extends AnyFlatSpecLike with Matchers {

    behavior of "Last"

    val tuple = (1, "hello", true, 0.23, Map( "hello" -> 500 ))

    it should "split a tuple at the end" in {
        Last.last( tuple ) shouldBe ((1, "hello", true, 0.23), Map( "hello" -> 500 ))
    }

}
