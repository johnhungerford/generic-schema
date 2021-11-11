package org.hungerford.generic.schema

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class SchemaProviderTest extends AnyFlatSpecLike with Matchers {

    behavior of "SchemaProvider"

    import org.hungerford.generic.schema.Primitives.given

    it should "provide a schema from an implicit if one exists" in {

        val intSch = SchemaProvider.schema[ Int ]
    }

    it should "provide a schema from derivation" in {
        case class Test( int : Int )

        val sch = SchemaProvider.schema[ Test ]

        sch.shape.size shouldBe 1
   }

}
