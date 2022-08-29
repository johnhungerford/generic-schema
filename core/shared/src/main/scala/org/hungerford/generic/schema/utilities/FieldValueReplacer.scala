package org.hungerford.generic.schema.utilities

import org.hungerford.generic.schema.product.field.FieldRetriever

trait FieldValueReplacer[Sel, R <: Tuple, RV <: Tuple] {
    type Inner

    def replace(fields: R, values: RV, newValue: Inner): RV
}

trait FieldValueReplacers1 {
    given nextReplacer[Sel, I, NonI, RVTail <: Tuple, Fld, RTail <: Tuple](
        using
        next: FieldValueReplacer.Aux[Sel, RTail, RVTail, I],
    ) : FieldValueReplacer[Sel, Fld *: RTail, NonI *: RVTail] with {
        type Inner = I

        def replace(fields: Fld *: RTail, values: NonI *: RVTail, newValue: I): NonI *: RVTail =
            values.head *: next.replace(fields.tail, values.tail, newValue)
    }
}

object FieldValueReplacer extends FieldValueReplacers1 {
    type Aux[Sel, R <: Tuple, RV <: Tuple, I] = FieldValueReplacer[Sel, R, RV] { type Inner = I }

    given emptyReplacer[Sel, I] : FieldValueReplacer[Sel, EmptyTuple, EmptyTuple] with {
        type Inner = I
        def replace(fields: EmptyTuple, values: EmptyTuple, newValue: I): EmptyTuple = EmptyTuple
    }

    given replaceHead[Sel <: Singleton, I, RVTail <: Tuple, Fld, RTail <: Tuple](
        using
        frt : FieldRetriever.Aux[Sel, Fld *: RTail, Fld],
    ) : FieldValueReplacer[Sel, Fld *: RTail, I *: RVTail] with {
        type Inner = I
        def replace(fields : Fld *: RTail, values : I *: RVTail, newValue : I): I *: RVTail =
            val tail: RVTail = values.tail
            newValue *: tail
    }
}
