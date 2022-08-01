package org.hungerford.generic.schema.primitives

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.types.TypeName

class PrimitivesTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "Option"

    it should "resolve a schema for option" in {
        import org.hungerford.generic.schema.Default.dsl.*
        import org.hungerford.generic.schema.primitives.Primitives.given

        case class TestOpt(i: Option[Int])
        val toSch = Schema.derived[TestOpt]
        val optSch = toSch("i").schema
        optSch.shape.subtypeDescriptions.tail.head.typeName shouldBe "Type"
    }

}
