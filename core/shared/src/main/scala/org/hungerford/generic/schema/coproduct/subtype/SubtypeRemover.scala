package org.hungerford.generic.schema.coproduct.subtype

trait SubtypeRemover[ N <: TypeName, R <: Tuple ] {
    type Out <: Tuple

    def remove( from : R ) : Out
}

trait LowPrioritySubtypeRemovers {
    given notFound[ N <: TypeName, Head, Tail <: Tuple, NextRes <: Tuple ](
        using
        next : SubtypeRemover.Aux[ N, Tail, NextRes ],
    ) : SubtypeRemover[ N, Head *: Tail ] with {
        type Out = Head *: NextRes

        def remove( from : Head *: Tail ) : Out = from.head *: next.remove( from.tail )
    }
}

object SubtypeRemover extends LowPrioritySubtypeRemovers {
    type Aux[ N <: TypeName, R <: Tuple, O ] = SubtypeRemover[ N, R ] { type Out = O }

    def apply[ N <: TypeName, R <: Tuple ](
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

    def remove[ N <: TypeName, R <: Tuple ](
        typeName : N,
        from : R,
    )(
        using
        str : SubtypeRemover[ N, R ]
    ) : str.Out = str.remove( from )
}
