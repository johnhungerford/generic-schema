package org.hungerford.generic.schema.types

import org.scalatest.flatspec.AnyFlatSpecLike

class TypeNameTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "TypeName"

    it should "exist for option" in {
        val tnOptionInt = summon[TypeName[Option[Int]]]
        tnOptionInt.name shouldBe "Option"
    }

}
