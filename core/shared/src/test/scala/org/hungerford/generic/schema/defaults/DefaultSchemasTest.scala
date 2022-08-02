package org.hungerford.generic.schema.defaults

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.types.TypeName

class DefaultSchemasTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "Option"

    it should "resolve a schema for option" in {
        import generic.schema.exports.*
        import org.hungerford.generic.schema.defaults.DefaultSchemas.given

        case class TestOpt(i: Option[Int])
        val toSch = Schema.derived[TestOpt]
        val optSch = toSch("i").schema
        optSch.shape.subtypeDescriptions.head.typeName shouldBe "Empty"
        optSch.shape.subtypeDescriptions.tail.head.typeName shouldBe "NonEmpty"
    }

}
