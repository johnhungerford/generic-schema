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
        case class Test( int: Int )

        val sch = SchemaProvider.schema[ Test ]

        sch.shape.size shouldBe 1
    }

    sealed trait Inner
    final case class Bottom( str : String ) extends Inner
    case class Middle( inner : Inner )
    case class Outer( middle : Middle )

    it should "provide a schema from extraction from a given schema" in {
        import org.hungerford.generic.schema.Default.dsl.*
        val outerSch = Schema.derived[ Outer ]
        import outerSch.given

        val bottomSch = SchemaProvider.schema[ Bottom ]
        bottomSch.shape.size shouldBe 1
        bottomSch.shape.fieldDescriptions.head.fieldName shouldBe "str"
    }

}
