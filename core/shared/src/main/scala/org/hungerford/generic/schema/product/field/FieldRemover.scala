package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.product.ProductShape

trait FieldRemover[ N <: FieldName, R <: Tuple ] {
    type Out <: Tuple
  
    def remove( fields : R ) : Out
}

trait LowPriorityFieldRemovers {
    type Aux[ N <: FieldName, R <: Tuple, O <: Tuple ] =
        FieldRemover[ N, R ] { type Out = O }

    given nextFieldRemover[ N <: FieldName, Head, Tail <: Tuple, Next <: Tuple ](
        using
        next : FieldRemover.Aux[ N, Tail, Next ],
    ) : FieldRemover.Aux[ N, Head *: Tail, Head *: Next ] = new FieldRemover[ N, Head *: Tail ] {
        type Out = Head *: Next

        def remove( fields : Head *: Tail ) : Head *: Next =
            fields.head *: next.remove( fields.tail )
    }
}

object FieldRemover extends LowPriorityFieldRemovers {
    given fieldRemover[ T, N <: FieldName, S, Tail <: Tuple ] : FieldRemover.Aux[ N, FieldDescription.Aux[ T, N, S ] *: Tail, Tail ] = new FieldRemover[ N, FieldDescription.Aux[ T, N, S ] *: Tail ] {
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
