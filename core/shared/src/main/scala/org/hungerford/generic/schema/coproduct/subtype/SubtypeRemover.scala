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

    given found[ N <: TypeName, SubT <: Subtype.Named[ N ], Tail <: Tuple ] : SubtypeRemover[ N, SubT *: Tail ] with {
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

trait SubtypeTypeRemover[ T, N <: Nat, R <: Tuple ] {
    type Out <: Tuple

    def remove( fields : R ) : Out
}

trait LowPrioritySubtypeTypeRemovers {
    given nextFieldRemover[ T , N <: Nat, Head, Tail <: Tuple, Res <: Tuple ](
        using
        next : SubtypeTypeRemover.Aux[ T, N, Tail, Res ],
    ) : SubtypeTypeRemover[ T, N, Head *: Tail ] with {
        type Out = Head *: Res

        override def remove( fields: Head *: Tail ): Head *: Res = fields.head *: next.remove( fields.tail )
    }
}

object SubtypeTypeRemover extends LowPrioritySubtypeTypeRemovers {
    type Aux[ T, N <: Nat, R <: Tuple, O <: Tuple ] =
        SubtypeTypeRemover[ T, N, R ] { type Out = O }

    given fieldRemoverByTypeZero[ T, SubT <: Subtype.Tpe[ T ], Tail <: Tuple ] : SubtypeTypeRemover[ T, Nat._0, SubT *: Tail ] with {
        type Out = Tail

        def remove( fields : SubT *: Tail ) : Tail = fields.tail
    }

    given fieldRemoverByTypeNonZero[ T, N <: Nat, DecN <: Nat, SubT <: Subtype.Tpe[ T ], Tail <: Tuple, Res <: Tuple ](
        using
        ev :  Nat.DecA[ N, DecN ],
        next : SubtypeTypeRemover.Aux[ T, DecN, Tail, Res ],
    ) : SubtypeTypeRemover[ T, N, SubT *: Tail ] with {
        type Out = SubT *: Res

        def remove( fields : SubT *: Tail ) : SubT *: Res = fields.head *: next.remove( fields.tail )
    }

}
