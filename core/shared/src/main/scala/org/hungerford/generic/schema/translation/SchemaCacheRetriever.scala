package org.hungerford.generic.schema.translation

import scala.annotation.implicitNotFound
import scala.compiletime.{erasedValue, error}

trait TypeCache { self =>
    type Cache <: Tuple
    type With[ T, A ] = TypeCache.Cached[ TypeCache.Aux[ Cache ], T, A ]

    def cache : Cache

    class AddCons[ T ]:
        def apply[ A ]( item: => A ) : With[ T, A ] =
            type OuterCache = Cache
            val outerInstance: TypeCache.Aux[ OuterCache ] = self
            new TypeCache {
                override type Cache = CI[ T, A ] *: OuterCache
                override def cache: CI[ T, A ] *: OuterCache = CI.of[ T ]( item ) *: outerInstance.cache
            }

    def add[ T ] : AddCons[ T ] = new AddCons[ T ]
    def extract[ T ]( using rt : TypeCacheRetriever[ TypeCache.Aux[ Cache ], T ] ) : CI[ T, rt.Out ] = rt.get( self )
    def extractValue[T, A]( using rt : TypeCacheRetriever.Aux[ TypeCache.Aux[ Cache ], T, A ] ) : CI[ T, A ] = rt.get( self )
}

object TypeCache {
    type Aux[ C <: Tuple ] = TypeCache { type Cache = C }
    type Empty = TypeCache.Aux[ EmptyTuple ]
    type Cached[ C <: TypeCache, T, A ] <: TypeCache = C match
        case TypeCache.Aux[ t ] => TypeCache.Aux[ CI[ T, A ] *: t ]

    val Empty : Empty =
        new TypeCache:
            override type Cache = EmptyTuple
            override def cache: EmptyTuple = EmptyTuple

    class Cons[ T ] {
        def apply[A](value: A): TypeCache.Aux[CI[T, A] *: EmptyTuple] =
            new TypeCache:
                override type Cache = CI[T, A] *: EmptyTuple
                override def cache: Cache = CI.of[T](value) *: EmptyTuple
    }

    def apply[ T ] : Cons[ T ] = new Cons[ T ]
}

trait CacheItem[ T ] {
    type Out

    def get : () => Out
}

@implicitNotFound("Could not find a cached value mapped to type ${T} in ${C}")
trait TypeCacheRetriever[ C <: TypeCache, T ] {
    type Out

    def get( from: C ) : CI[ T, Out ]
}

trait TypeCacheRetriever1 {
    given next[ NotTr, Next <: Tuple, T, O ](
        using
        nextTrRt: TypeCacheRetriever.Aux[ TypeCache.Aux[ Next ], T, O ],
    ): TypeCacheRetriever[ TypeCache.Aux[ NotTr *: Next ], T ] with {
        type Out = O

        def get( from: TypeCache.Aux[ NotTr *: Next ] ): CI[ T, O ] =
            val nextCache = new TypeCache:
                override type Cache = Next
                override def cache: Next = from.cache.tail
            nextTrRt.get( nextCache )
    }
}

object TypeCacheRetriever extends TypeCacheRetriever1 {
    type Aux[ C <: TypeCache, T, O ] = TypeCacheRetriever[ C, T ] { type Out = O }

    inline given isHead[ T, NextCache <: Tuple, O ]: TypeCacheRetriever.Aux[ TypeCache.Aux[ CI[ T, O ] *: NextCache ], T, O ] = {
        type Trans = CI[ T, O ] *: NextCache

        new TypeCacheRetriever[ TypeCache.Aux[ Trans ], T ] {
            type Out = O
            def get( from: TypeCache.Aux[ Trans ] ): CI[ T, O ] = from.cache.head
        }
    }
}

case class CI[ T, O ]( override val get : () => O ) extends CacheItem[ T ] { type Out = O }

object CI {
    class Cons[ T ] {
        def apply[ O ]( get : => O ) : CI[ T, O ] = CI[ T, O ]( () => get )
    }

    def of[ T ] : Cons[ T ] = new Cons[ T ]
}

trait Cacher[ Cache <: TypeCache, T ] {
    type In
    type Out
    type NewTrans = TypeCache.Cached[ Cache, T, Out ]

    def genCachedValue( value : In ) : Out

    def cached( value : In, cache : Cache ) : NewTrans = cache.add[T]( genCachedValue( value ) ).asInstanceOf[ NewTrans ]
}

object Cacher {
    type AuxO[ Cache <: TypeCache, T, O ] = Cacher[ Cache, T ] { type Out = O }
    type AuxI[ Cache <: TypeCache, T, I ] = Cacher[ Cache, T ] { type In = I}
    type AuxIO[ Cache <: TypeCache, T, I, O ] = Cacher[ Cache, T ] { type In = I; type Out = O }
    type AuxNT[ Cache <: TypeCache, T, NT ] = Cacher[ Cache, T ] { type NewTrans = NT }
    type AuxINT[ Cache <: TypeCache, T, I, NT ] = Cacher[ Cache, T ] { type In = I; type NewTrans = NT }
}
