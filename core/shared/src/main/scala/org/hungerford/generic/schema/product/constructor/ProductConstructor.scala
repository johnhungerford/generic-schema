package org.hungerford.generic.schema.product.constructor

import org.hungerford.generic.schema.types.Last

trait ProductConstructor[ C, R <: Tuple, AF, T ] {
    def convert( from : C ) : (R, Map[ String, AF ]) => T

    def construct(
        from : C,
    )(
        fields : R,
        additionalFields : Map[ String, AF ] = Map.empty[ String, AF ],
    ) : T = convert( from )( fields, additionalFields )
}

object ProductConstructor {
    given standard[ R <: Tuple, AF, T ] : ProductConstructor[ (R, Map[ String, AF ]) => T, R, AF, T ] with {
        override def convert( from : (R, Map[ String, AF ]) => T ) : (R, Map[ String, AF ]) => T = from
    }

    given standardWithoutAF[ R <: Tuple, T ] : ProductConstructor[ R => T, R, Nothing, T ] with {
        override def convert( from : R => T ) : (R, Map[ String, Nothing ]) => T = {
            ( fields : R, _ : Map[ String, Nothing ] ) => from( fields )
        }
    }

    given tupled[ R <: Tuple , AF, T ] :
      ProductConstructor[ Tuple.Concat[ R, Map[ String, AF ] *: EmptyTuple ] => T, R, AF, T ] with {
        def convert( from : Tuple.Concat[ R, Map[ String, AF ] *: EmptyTuple ] => T ) : (R, Map[ String, AF ]) => T = {
            ( fields : R, additionalFields : Map[ String, AF ] ) => {
                val tupledParameters = fields ++ (additionalFields *: EmptyTuple)
                from( tupledParameters )
            }
        }
    }

}

trait ProductDeconstructor[ T, R <: Tuple, AF, Res ] {
    def convert( from : T => Res ) : T => (R, Map[ String, AF ])

    def deconstruct( from : T => Res )(
        value : T,
    ) : (R, Map[ String, AF ]) = convert( from )( value )
}

object ProductDeconstructor {
    given standard[ T, R <: Tuple, AF ] :
        ProductDeconstructor[ T, R, AF, (R, Map[ String, AF ]) ] with {
        def convert( from : T => (R, Map[ String, AF ]) ) :
            T => (R, Map[ String, AF ]) = from
    }

    given noAF[ T, R <: Tuple ] : ProductDeconstructor[ T, R, Nothing, R ] with {
        def convert( from : T => R ) : T => (R, Map[ String, Nothing ]) = {
            ( value : T ) => {
                val res : R = from( value )
                (res, Map.empty[ String, Nothing ])
            }
        }
    }

    given tupled[ T, R <: Tuple, AF, Res <: Tuple ](
        using
        ls : Last.Aux[ Res, R, Map[ String, AF ] ],
    ) : ProductDeconstructor[ T, R, AF, Res ] with {
        def convert( from : T => Res ) : T => (R, Map[ String, AF ]) = {
            ( value : T ) => {
                val res : Res = from( value )
                ls.last( res )
            }
        }
    }

}
