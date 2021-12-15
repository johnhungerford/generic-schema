package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.Primitive
import org.hungerford.generic.schema.coproduct.subtype.SubtypeCase
import org.scalatest.flatspec.AnyFlatSpecLike

class CoproductSchemaBuilderTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "CoproductSchemaBuilder"

    trait SomeTrait

    it should "be able to add a subtype" in {
        val csb = CoproductSchemaBuilder.empty[ SomeTrait ]
          .addSubtype[ Int ]( SubtypeCase[ Int, "int", Unit ]( "int", Primitive[ Int ]() ) )
          .addSubtype[ String ]( SubtypeCase[ String, "str", Unit ]( "str", Primitive[ String ]() ) )
    }

}
