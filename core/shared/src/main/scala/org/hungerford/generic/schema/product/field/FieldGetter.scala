package org.hungerford.generic.schema.product.field

trait FieldGetter[ N <: FieldName, R <: Tuple, RV <: Tuple ] {
    type Out

    def get( from : RV ) : Out
}

object FieldGetter {
    type Aux[ N <: FieldName, R <: Tuple, RV <: Tuple, O ] = FieldGetter[ N, R, RV ] { type Out = O }

    given [ T, F, N <: FieldName, S, RTail <: Tuple, RVHead, RVTail <: Tuple ] :
        FieldGetter.Aux[ N, Field[ T, F, N, S ] *: RTail, RVHead *: RVTail, RVHead ] = {
        new FieldGetter[ N, Field[ T, F, N, S ] *: RTail, RVHead *: RVTail ] {
            type Out = RVHead

            def get( from : RVHead *: RVTail ) : RVHead = from.head
        }
    }

    given [ N <: FieldName, RHead, RTail <: Tuple, RVHead, RVTail <: Tuple ](
        using
        next : FieldGetter[ N, RTail, RVTail ],
    ) : FieldGetter.Aux[ N, RHead *: RTail, RVHead *: RVTail, next.Out ] = {
        new FieldGetter[ N, RHead *: RTail, RVHead *: RVTail ] {
            type Out = next.Out

            def get( from : RVHead *: RVTail ) : next.Out = next.get( from.tail )
        }
    }
    
    def get[ N <: FieldName, R <: Tuple, RV <: Tuple ](
        from : RV,
        field : N,
        informedBy : R,
    )(
        using
        getter : FieldGetter[ N, R, RV ],
    ) : getter.Out = getter.get( from )
}
