package org.hungerford.generic.schema.types

trait Last[ R <: Tuple ] {
    type Head <: Tuple
    type Tail

    def last( of : R ) : (Head, Tail)
}

object Last {
    type Aux[ R <: Tuple, H <: Tuple, T ] =
        Last[ R ] { type Head = H; type Tail = T }

    given [ H ] : Last.Aux[ H *: EmptyTuple, EmptyTuple, H ] = {
        new Last[ H *: EmptyTuple ] {
            type Head = EmptyTuple
            type Tail = H

            def last( of : H *: EmptyTuple ) : (EmptyTuple, H) = (EmptyTuple, of.head)
        }
    }

    given [ H, T <: Tuple, NextH <: Tuple, NextT ](
        using
        nextLast : Last.Aux[ T, NextH, NextT ]
    ) : Last.Aux[ H *: T, H *: NextH, NextT ] = new Last[ H *: T ] {
        type Head = H *: NextH
        type Tail = NextT

        def last( of : H *: T ) : (H *: NextH, NextT) = {
            val (nextH, nextT) = nextLast.last( of.tail )
            (of.head *: nextH, nextT)
        }
    }

    def last[ R <: Tuple ]( tuple : R )( using lst : Last[ R ] ) : (lst.Head, lst.Tail) = lst.last( tuple )
}
