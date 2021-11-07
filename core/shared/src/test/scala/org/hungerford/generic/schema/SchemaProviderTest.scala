package org.hungerford.generic.schema

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class SchemaProviderTest extends AnyFlatSpecLike with Matchers {

    behavior of "SchemaProvider"

    it should "provide a schema from derivation" in {
        case class Test( int : Int )

        val sch = SchemaProvider.schema[ Test ]

        sch.shape.fields shouldBe Set( "int" )
    }

}
