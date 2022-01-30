package org.hungerford.generic.schema.types

import scala.compiletime.ops.int.{>, -, <=}
import scala.util.NotGiven

trait Remover[ I, R <: Tuple ] {
    type Out <: Tuple

    def remove( tuple : R ) : Out
}

trait LowPriorityRemovers {
    given continue[ I <: Nat, DecI <: Nat, Head, Tail <: Tuple, Res <: Tuple ](
        using
        ev : Nat.DecA[ I, DecI ],
        next : Remover.Aux[ DecI, Tail, Res ],
    ) : Remover[ I, Head *: Tail ] with {
        type Out = Head *: Res

        override def remove( tuple: Head *: Tail ): Out = tuple.head *: next.remove( tuple.tail )
    }

}

object Remover extends LowPriorityRemovers {
    type Aux[ I, R <: Tuple, O ] = Remover[ I, R ] { type Out = O }

    given remove[ I <: Nat, Head, Tail <: Tuple ] : Remover[ _0, Head *: Tail ] with {
        type Out = Tail

        override def remove( tuple: Head *: Tail ): Out = tuple.tail
    }

    given equiv[ I <: Int, N <: Nat, R <: Tuple, Res <: Tuple ](
        using
        ev : Nat.IntA[ I, N ],
        rm : Remover.Aux[ N, R, Res ],
    ) : Remover[ I, R ] with {
        type Out = Res

        override def remove( tuple: R ): Out = rm.remove( tuple )
    }

    sealed class RM[ I ] {
        def apply[ R <: Tuple ](
            tuple : R,
        )(
            using
            rm : Remover[ I, R ],
        ) : rm.Out = rm.remove( tuple )
    }

    def remove[ I ] : RM[ I ] = new RM[ I ]
}

