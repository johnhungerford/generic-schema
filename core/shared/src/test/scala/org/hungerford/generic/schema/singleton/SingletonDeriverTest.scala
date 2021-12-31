package org.hungerford.generic.schema.singleton

import org.scalatest.flatspec.AnyFlatSpecLike

class SingletonDeriverTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "SingletonDeriver"

    case object TestSingleton

    it should "derive a schema for a case object" in {
        val deriver = summon[ SingletonDeriver[ TestSingleton.type ] ]
        deriver.derive shouldBe SingletonShape[ TestSingleton.type, "TestSingleton" ]( "TestSingleton", TestSingleton )
    }

}
