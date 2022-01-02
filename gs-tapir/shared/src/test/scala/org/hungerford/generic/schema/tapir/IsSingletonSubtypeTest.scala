package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.ComplexSchema
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, SubtypeCase}
import org.hungerford.generic.schema.singleton.SingletonShape
import org.scalatest.flatspec.AnyFlatSpecLike

class IsSingletonSubtypeTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "IsSingletonSubtype"



    it should "be summonable for a subtype of a singleton" in {
        val subtype = SubtypeCase[ Int, 1, Unit, Nothing, Unit, "hello", SingletonShape[ 1, "singleton-name" ] ]( "hello", ComplexSchema( SingletonShape[ 1, "singleton-name" ]( "singleton-name", 1 ) ), v => v, _ => Some( 1 ), () )
        summon[ IsSingletonSubtype[ Subtype.Aux[ Int, 1, Unit, Nothing, Unit, "hello", SingletonShape[ 1, "singleton-name" ] ] ] ]
        summon[ IsSingletonSubtype[ SubtypeCase[ Int, 1, Unit, Nothing, Unit, "hello", SingletonShape[ 1, "singleton-name" ] ] ] ]
        summon[ IsSingletonSubtype[ subtype.type ] ]
    }

}
