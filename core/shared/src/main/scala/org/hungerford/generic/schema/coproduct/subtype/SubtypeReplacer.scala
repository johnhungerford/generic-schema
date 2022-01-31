package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.types.{Nat, Replacer}

trait SubtypeReplacer[ N <: Singleton, NewSubT, R <: Tuple ] {
    type Out

    def replace( subtypes : R, withSubtype : NewSubT ) : Out
}

trait LowPrioritySubtypeReplacer {
    given notFound[ N <: TypeName, Head, Tail <: Tuple, NewSubT, NextRes <: Tuple ](
        using
        next : SubtypeReplacer.Aux[ N, NewSubT, Tail, NextRes ],
    ) : SubtypeReplacer[ N, NewSubT, Head *: Tail ] with {
        type Out = Head *: NextRes

        def replace( subtypes: Head *: Tail, withSubtype: NewSubT ): Out =
            subtypes.head *: next.replace( subtypes.tail, withSubtype )
    }
}

object SubtypeReplacer extends LowPrioritySubtypeReplacer {
    type Aux[ N <: Singleton, NewSubT, R <: Tuple, O ] = SubtypeReplacer[ N, NewSubT, R ] { type Out = O }

    given found[ N <: TypeName, NewSubT, OldT, OldST, OldD, OldDN, OldDV, OldS, Tail <: Tuple  ] :
      SubtypeReplacer[ N, NewSubT, Subtype.Aux[ OldT, OldST, OldD, OldDN, OldDV, N, OldS ] *: Tail ] with {
        type Out = NewSubT *: Tail

        override def replace(
            subtypes: Subtype.Aux[ OldT, OldST, OldD, OldDN, OldDV, N, OldS ] *: Tail,
            withSubtype: NewSubT
        ) : Out = withSubtype *: subtypes.tail
    }

    given subtypeReplacer[ I <: Int & Singleton, N <: Nat, NewSubT, R <: Tuple, Res ](
        using
        ev : Nat.IntA[ I, N ],
        rp : Replacer.Aux[ N, R, NewSubT, Res ],
    ) : SubtypeReplacer[ I, NewSubT, R ] with {
        type Out = Res

        override def replace( subtypes: R, withSubtype: NewSubT ): Res =
            rp.replace( subtypes, withSubtype )
    }

    def replace[ N <: Singleton, NewSubT, R <: Tuple ](
        typeName : N,
        from : R,
        withSubtype : NewSubT,
    )(
        using
        strp : SubtypeReplacer[ N, NewSubT, R ],
    ) : strp.Out = strp.replace( from, withSubtype )
}
