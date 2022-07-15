package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.Schema

import scala.compiletime.{error, erasedValue}

trait CacheItem[ T ] {
    type Out

    def get : () => Out
}

case class CI[ T, O ]( override val get : () => O ) extends CacheItem[ T ] { type Out = O }

object CI {
    class Cons[ T ] {
        def apply[ O ]( get : => O ) : CI[ T, O ] = CI[ T, O ]( () => get )
    }

    def of[ T ] : Cons[ T ] = new Cons[ T ]
}

trait SchemaCacheRetriever[ Trans <: Tuple, T ] {
    type Out

    def getter( from : Trans ): CI[ T, Out ]
}

trait SchemaCacheRetriever1 {
    type Aux[ Trans <: Tuple, T, O ] = SchemaCacheRetriever[ Trans, T ] { type Out = O }

    given next[ NotTr, Next <: Tuple, T, O ](
        using
        nextTrRt: SchemaCacheRetriever.Aux[ Next, T, O ],
    ): SchemaCacheRetriever[ NotTr *: Next, T ] with {
        type Out = O

        def getter( from: NotTr *: Next ): CI[ T, O ] =
            nextTrRt.getter( from.tail )
    }
}

object SchemaCacheRetriever extends SchemaCacheRetriever1 {

    inline given isHead[ T, NextCache <: Tuple, O ]: SchemaCacheRetriever.Aux[ CI[ T, O ] *: NextCache, T, O ] = {
        type Trans = CI[ T, O ] *: NextCache

        new SchemaCacheRetriever[ Trans, T ] {
            type Out = O
            def getter( from: Trans ): CI[ T, O ] = from.head
        }
    }
}

trait Cached[ Trans <: Tuple, T ] {
    type In
    type Out
    type NewTrans = CI[ T, Out ] *: Trans

    def cacheItem( value : In ) : Out

    def cached( value : In, trans : Trans ) : NewTrans = CI.of[ T ]( cacheItem( value ) ) *: trans
}

object Cached {
    type AuxO[ Trans <: Tuple, T, O ] = Cached[ Trans, T ] { type Out = O }
    type AuxI[ Trans <: Tuple, T, I ] = Cached[ Trans, T ] { type In = I}
    type AuxIO[ Trans <: Tuple, T, I, O ] = Cached[ Trans, T ] { type In = I; type Out = O }
    type AuxNT[ Trans <: Tuple, T, NT ] = Cached[ Trans, T ] { type NewTrans = NT }
    type AuxINT[ Trans <: Tuple, T, I, NT ] = Cached[ Trans, T ] { type In = I; type NewTrans = NT }
}
