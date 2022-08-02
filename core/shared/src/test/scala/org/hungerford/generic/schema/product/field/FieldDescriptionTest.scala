package org.hungerford.generic.schema.product.field

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import org.hungerford.generic.schema.Schema
import generic.schema.exports.*

class FieldDescriptionTest extends AnyFlatSpecLike with Matchers {

    behavior of "FieldDescriptionBuilder"

    it should "be able to rebuild a schema before field name has been set" in {
        case class Inner( int : Int )
        case class Outer( inner : Inner )

        val fd = FieldBuilder[ Int, Outer ]
          .extractor( v => Outer( Inner( v ) ) )
          .fromSchema( Schema.derived )
          .rebuildSchema(
              _.rebuildField( "inner" )(
                  _.name( "inner_field" )
                    .build
              ).build
          )
          .name( "outer_field" )
          .build

        fd.fieldName shouldBe "outer_field"
        fd.schema.shape.fieldDescriptions.size shouldBe 1
        fd.schema.shape.fieldDescriptions.head.fieldName shouldBe "inner_field"
    }

}
