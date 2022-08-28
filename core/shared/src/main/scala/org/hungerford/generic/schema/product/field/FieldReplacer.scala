package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.types.{Nat, Replacer, Retriever}

trait FieldReplacer[ N <: Singleton, R <: Tuple, NewT, NewF, NewN <: FieldName, NewS ] {
    type Out <: Tuple

    def replace( fields : R, withField : Field[ NewT, NewF, NewN, NewS ] ) : Out
}

trait LowPriorityFieldReplacers {
    given [ N <: FieldName, NewT, NewF, NewN <: FieldName, NewS, Head, Tail <: Tuple, Next <: Tuple ](
        using
        next : FieldReplacer.Aux[ N, Tail, NewT, NewF, NewN, NewS, Next  ]
    ) : FieldReplacer.Aux[ N, Head *: Tail, NewT, NewF, NewN, NewS, Head *: Next ] = {
        new FieldReplacer[ N, Head *: Tail, NewT, NewF, NewN, NewS ] {
            type Out = Head *: Next

            def replace(
                fields : Head *: Tail,
                withField : Field[ NewT, NewF, NewN, NewS ],
            ) : Head *: Next = fields.head *: next.replace( fields.tail, withField )
        }
    }
}

object FieldReplacer extends LowPriorityFieldReplacers {
    type Aux[ N <: Singleton, R <: Tuple, NewT, NewF, NewN <: FieldName, NewS, O <: Tuple ] =
        FieldReplacer[ N, R, NewT, NewF, NewN, NewS ] { type Out = O }

    given [ OldT, OldF, OldN <: FieldName, OldS, NewT, NewF, NewN <: FieldName, NewS, Tail <: Tuple ] : FieldReplacer.Aux[ OldN, Field[ OldT, OldF, OldN, OldS ] *: Tail, NewT, NewF, NewN, NewS, Field[ NewT, NewF, NewN, NewS ] *: Tail ] = {
        new FieldReplacer[  OldN, Field[ OldT, OldF, OldN, OldS ] *: Tail, NewT, NewF, NewN, NewS ] {
            type Out = Field[ NewT, NewF, NewN, NewS ] *: Tail

            def replace(
                fields : Field[ OldT, OldF, OldN, OldS ] *: Tail,
                withField : Field[ NewT, NewF, NewN, NewS ],
            ) : Field[ NewT, NewF, NewN, NewS ] *: Tail =
                val tail : Tail = fields.tail
                withField *: tail
        }
    }

    given fieldReplacerByIndex[ I <: Int & Singleton, R <: Tuple, NewT, NewF, NewN <: FieldName, NewS, Res <: Tuple ](
        using
        rp : Replacer.Aux[ I, R, Field[ NewT, NewF, NewN, NewS ], Res ],
    ) : FieldReplacer[ I, R, NewT, NewF, NewN, NewS ] with {
        type Out = Res

        def replace(
            fields : R,
            withField : Field[ NewT, NewF, NewN, NewS ],
        ) : Res = rp.replace( fields, withField )
    }

    def replace[ N <: Singleton, R <: Tuple, NewT, NewF, NewN <: FieldName, NewS ](
        identifier : N,
        from : R,
        withField : Field[ NewT, NewF, NewN, NewS ],
    )(
        using
        replacer : FieldReplacer[ N, R, NewT, NewF, NewN, NewS ],
    ) : replacer.Out = replacer.replace( from, withField )
}

trait FieldTypeReplacer[ T, N <: Nat, R <: Tuple, NewT, NewF, NewN <: FieldName, NewS ] {
    type Out <: Tuple

    def replace( fields : R, withField : Field[ NewT, NewF, NewN, NewS ] ) : Out
}

trait LowPriorityFieldTypeReplacers {
    given [ F, N <: Nat, NewT, NewF, NewN <: FieldName, NewS, Head, Tail <: Tuple, Next <: Tuple ](
        using
        next : FieldTypeReplacer.Aux[ F, N, Tail, NewT, NewF, NewN, NewS, Next  ]
    ) : FieldTypeReplacer.Aux[ F, N, Head *: Tail, NewT, NewF, NewN, NewS, Head *: Next ] = {
        new FieldTypeReplacer[ F, N, Head *: Tail, NewT, NewF, NewN, NewS ] {
            type Out = Head *: Next

            def replace(
                fields : Head *: Tail,
                withField : Field[ NewT, NewF, NewN, NewS ],
            ) : Head *: Next = fields.head *: next.replace( fields.tail, withField )
        }
    }
}

object FieldTypeReplacer extends LowPriorityFieldTypeReplacers {
    type Aux[ F, N <: Nat, R <: Tuple, NewT, NewF, NewN <: FieldName, NewS, O <: Tuple ] =
        FieldTypeReplacer[ F, N, R, NewT, NewF, NewN, NewS ] { type Out = O }

    given [ F, Fld <: Field.Tpe[ F ], NewT, NewF, NewN <: FieldName, NewS, Tail <: Tuple ] : FieldTypeReplacer.Aux[ F, Nat._0, Fld *: Tail, NewT, NewF, NewN, NewS, Field[ NewT, NewF, NewN, NewS ] *: Tail ] = {
        new FieldTypeReplacer[ F, Nat._0, Fld *: Tail, NewT, NewF, NewN, NewS ] {
            type Out = Field[ NewT, NewF, NewN, NewS ] *: Tail

            def replace(
                fields : Fld *: Tail,
                withField : Field[ NewT, NewF, NewN, NewS ],
            ) : Field[ NewT, NewF, NewN, NewS ] *: Tail =
                val tail: Tail = fields.tail
                withField *: tail
        }
    }

    given [ F, N <: Nat, DecN <: Nat, Fld <: Field.Tpe[ F ], NewT, NewF, NewN <: FieldName, NewS, Tail <: Tuple, Next <: Tuple ](
        using
        ev : Nat.DecA[ N, DecN ],
        next : FieldTypeReplacer.Aux[ F, DecN, Tail, NewT, NewF, NewN, NewS, Next ],
    ) : FieldTypeReplacer.Aux[ F, N, Fld *: Tail, NewT, NewF, NewN, NewS, Fld *: Next ] = {
        new FieldTypeReplacer[ F, N, Fld *: Tail, NewT, NewF, NewN, NewS ] {
            type Out = Fld *: Next

            def replace(
                fields : Fld *: Tail,
                withField : Field[ NewT, NewF, NewN, NewS ],
            ) : Fld *: Next = fields.head *: next.replace( fields.tail, withField )
        }
    }
}

