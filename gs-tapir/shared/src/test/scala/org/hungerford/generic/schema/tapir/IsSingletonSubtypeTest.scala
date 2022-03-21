package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.ComplexSchema
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype}
import org.hungerford.generic.schema.singleton.SingletonShape
import org.scalatest.flatspec.AnyFlatSpecLike

class IsSingletonSubtypeTest extends AnyFlatSpecLike with org.scalatest.matchers.should.Matchers {

    behavior of "IsSingletonSubtype"

    it should "be summonable for a subtype of a singleton" in {
        val subtype = Subtype[ Int, 1, Unit, Unit, Unit, "hello", SingletonShape[ 1, "singleton-name" ] ]( "hello", ComplexSchema( SingletonShape[ 1, "singleton-name" ]( "singleton-name", 1 ) ), v => v, _ => Some( 1 ), (), () )
        summon[ IsSingletonSubtype[ Subtype[ Int, 1, Unit, Unit, Unit, "hello", SingletonShape[ 1, "singleton-name" ] ] ] ]
        summon[ IsSingletonSubtype[ subtype.type ] ]
    }

    it should "be summonable for a lazy subtype when the given schema is a singleton subtype" in {
        import org.hungerford.generic.schema.Default.dsl.*

        given singletonSchema : Schema.Aux[ 1, SingletonShape[ 1, "singleton-name" ] ] =
            ComplexSchema( SingletonShape[ 1, "singleton-name" ]( "singleton-name", 1 ) )

        val subtype = LazySubtype[ Int, 1, Unit, Unit, Unit, "hello" ]( "hello", v => v, _ => Some( 1 ), (), () )
        summon[ IsSingletonSubtype[ LazySubtype[ Int, 1, Unit, Unit, Unit, "hello" ] ] ]
        summon[ IsSingletonSubtype[ subtype.type ] ]
    }

}
