package org.hungerford.generic.schema

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class SchemaProviderTest extends AnyFlatSpecLike with Matchers {

    behavior of "SchemaProvider"

    it should "provide a schema from an implicit if one exists" in {
        import org.hungerford.generic.schema.primitives.Primitives.given
        val intSch = SchemaProvider.schema[ Int ]
        intSch.genericDescription.nonEmpty shouldBe true
    }

    it should "provide a schema from derivation" in {
        case class Test( int : Int )

        val sch = SchemaProvider.schema[ Test ]

        sch.shape.size shouldBe 1
   }

}
