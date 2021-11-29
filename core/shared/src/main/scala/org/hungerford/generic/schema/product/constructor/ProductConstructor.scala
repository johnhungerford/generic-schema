package org.hungerford.generic.schema.product.constructor

trait ProductConstructor[ C, R <: Tuple, AF, T ] {
    def convert( from : C ) : (R, Map[ String, AF ]) => T

    def construct( from : C )( fields : R, additionalFields : Map[ String, AF ] ) : T =
        convert( from )( fields, additionalFields )
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
