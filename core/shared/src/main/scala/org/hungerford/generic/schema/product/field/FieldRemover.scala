package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.product.ProductShape

trait FieldRemover[ N <: FieldName, R <: Tuple ] {
    type Out <: Tuple
  
    def remove( fields : R ) : Out
}

trait LowPriorityFieldRemovers {
    type Aux[ N <: FieldName, R <: Tuple, O <: Tuple ] =
        FieldRemover[ N, R ] { type Out = O }

    given nextFieldRemover[ N <: FieldName, Head, Tail <: Tuple ](
        using
        next : FieldRemover[ N, Tail ],
    ) : FieldRemover[ N, Head *: Tail ] with {
        type Out = Head *: next.Out

        def remove( fields : Head *: Tail ) : Head *: next.Out =
            fields.head *: next.remove( fields.tail )
    }
}

object FieldRemover extends LowPriorityFieldRemovers {
    given fieldRemover[ T, N <: FieldName, S, Tail <: Tuple ] : FieldRemover[ N, FieldDescription.Aux[ T, N, S ] *: Tail ] with {
        type Out = Tail

        def remove( fields : FieldDescription.Aux[ T, N, S ] *: Tail ) : Tail = fields.tail
    }

    def remove[ N <: FieldName, R <: Tuple, Res <: Tuple ](
        fieldName : N,
        from : R,
    )(
        using
        rm : FieldRemover[ N, R ],
    ) : rm.Out = rm.remove( from )
}
