package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.{Schema, Primitive}
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, SubtypeCase}
import org.scalatest.flatspec.AnyFlatSpecLike

class CoproductSchemaBuilderTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "CoproductSchemaBuilder"

    case class SomeTrait( value : Any )

    it should "be able to add a subtype" in {
        val csb = CoproductSchemaBuilder.empty[ SomeTrait ]
          .addSubtype( SubtypeCase[ SomeTrait, Int, Unit, Nothing, Unit, "int",  Unit ]( "int", Primitive[ Int ](), SomeTrait.apply, () ) )
          .addSubtype( SubtypeCase[ SomeTrait, String, Unit, Nothing, Unit, "str", Unit ]( "str", Primitive[ String ](), SomeTrait.apply, () ) )
          .build

        summon[ csb.type <:< Schema.Aux[ SomeTrait, CoproductShape[ SomeTrait, (Subtype.Aux[ SomeTrait, Int, Unit, Nothing, Unit, "int",  Unit ], Subtype.Aux[ SomeTrait, String, Unit, Nothing, Unit, "str", Unit ]), (Int, String), Unit, Nothing ] ] ]
    }

}
