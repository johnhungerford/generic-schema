package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.types.{Nat, Remover}

trait SubtypeRemover[ N <: Singleton, R <: Tuple ] {
    type Out <: Tuple

    def remove( from : R ) : Out
}

trait LowPrioritySubtypeRemovers {
    given notFound[ N <: Singleton, Head, Tail <: Tuple, NextRes <: Tuple ](
        using
        next : SubtypeRemover.Aux[ N, Tail, NextRes ],
    ) : SubtypeRemover[ N, Head *: Tail ] with {
        type Out = Head *: NextRes

        def remove( from : Head *: Tail ) : Out = from.head *: next.remove( from.tail )
    }
}

object SubtypeRemover extends LowPrioritySubtypeRemovers {
    type Aux[ N <: Singleton, R <: Tuple, O ] = SubtypeRemover[ N, R ] { type Out = O }

    def apply[ N <: Singleton, R <: Tuple ](
        using
        str : SubtypeRemover[ N, R ],
    ) : SubtypeRemover.Aux[ N, R, str.Out ] = str

    given found[ N <: TypeName, SubT, Tail <: Tuple ](
        using
        ev : SubtypeOfName[ N, SubT ],
    ) : SubtypeRemover[ N, SubT *: Tail ] with {
        type Out = Tail

         def remove( from: SubT *: Tail ): Out = from.tail
    }

    given subtypeRemoverByIndex[ I <: Int & Singleton, N <: Nat, R <: Tuple, Res <: Tuple ](
        using
        ev : Nat.IntA[ I, N ],
        rp : Remover.Aux[ N, R, Res ],
    ) : SubtypeRemover[ I, R ] with {
        type Out = Res

        override def remove( from: R ) : Res =
            rp.remove( from )
    }

    def remove[ N <: Singleton, R <: Tuple ](
        typeName : N,
        from : R,
    )(
        using
        str : SubtypeRemover[ N, R ]
    ) : str.Out = str.remove( from )
}
