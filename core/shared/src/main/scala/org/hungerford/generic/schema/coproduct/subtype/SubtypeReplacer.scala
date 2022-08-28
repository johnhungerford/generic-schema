package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.selector.TypeSelector
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
      SubtypeReplacer[ N, NewSubT, Subtype[ OldT, OldST, OldD, OldDN, OldDV, N, OldS ] *: Tail ] with {
        type Out = NewSubT *: Tail

        override def replace(
            subtypes: Subtype[ OldT, OldST, OldD, OldDN, OldDV, N, OldS ] *: Tail,
            withSubtype: NewSubT
        ) : Out =
            val tail: Tail = subtypes.tail
            withSubtype *: tail
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

trait SubtypeTypeReplacer[ T, N <: Nat, NewSubT, R <: Tuple ] {
    type Out

    def replace( subtypes : R, withSubtype : NewSubT ) : Out
}

trait LowPrioritySubtypeTypeReplacer {
    given notFound[ T, N <: Nat, Head, Tail <: Tuple, NewSubT, NextRes <: Tuple ](
        using
        next : SubtypeTypeReplacer.Aux[ T, N, NewSubT, Tail, NextRes ],
    ) : SubtypeTypeReplacer[ T, N, NewSubT, Head *: Tail ] with {
        type Out = Head *: NextRes

        def replace( subtypes: Head *: Tail, withSubtype: NewSubT ): Out =
            subtypes.head *: next.replace( subtypes.tail, withSubtype )
    }
}

object SubtypeTypeReplacer extends LowPrioritySubtypeTypeReplacer {
    type Aux[ T, N <: Nat, NewSubT, R <: Tuple, O ] = SubtypeTypeReplacer[ T, N, NewSubT, R ] { type Out = O }

    given found[ ST, NewSubT, OldT, OldD, OldDN, OldDV, OldN <: TypeName, SubT <: Subtype.OrLazy[ OldT, ST, OldD, OldDN, OldDV, OldN ], Tail <: Tuple  ] :
      SubtypeTypeReplacer[ ST, Nat._0, NewSubT, SubT *: Tail ] with {
        type Out = NewSubT *: Tail

        override def replace(
            subtypes: SubT *: Tail,
            withSubtype: NewSubT
        ) : Out =
            val tail : Tail = subtypes.tail
            withSubtype *: tail
    }

    given foundNext[ ST, N <: Nat, DecN <: Nat, NewSubT, T, D, DN, DV, TN <: TypeName, SubT <: Subtype.OrLazy[ T, ST, D, DN, DV, TN ], Tail <: Tuple, Next <: Tuple ](
        using
        ev : Nat.DecA[ N, DecN ],
        next : SubtypeTypeReplacer.Aux[ ST, DecN, NewSubT, Tail, Next ],
    ) : SubtypeTypeReplacer[ ST, N, NewSubT, SubT *: Tail ] with {
        type Out = SubT *: Next

        override def replace(
            subtypes: SubT *: Tail,
            withSubtype: NewSubT
        ) : Out = subtypes.head *: next.replace( subtypes.tail, withSubtype )
    }

    def replace[ T, N <: Nat, NewSubT, R <: Tuple ](
        selector : TypeSelector[ T, N ],
        from : R,
        withSubtype : NewSubT,
    )(
        using
        strp : SubtypeTypeReplacer[ T, N, NewSubT, R ],
    ) : strp.Out = strp.replace( from, withSubtype )
}
