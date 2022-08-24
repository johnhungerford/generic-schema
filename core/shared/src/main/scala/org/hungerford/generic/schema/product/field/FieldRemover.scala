package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.selector.TypeSelector
import org.hungerford.generic.schema.types.{Minus, Nat, Remover, Replacer, Retriever}

trait FieldRemover[ N <: Singleton, R <: Tuple ] {
    type Out <: Tuple
  
    def remove( fields : R ) : Out
}

trait LowPriorityFieldRemovers {

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
    type Aux[ N <: Singleton, R <: Tuple, O <: Tuple ] =
        FieldRemover[ N, R ] { type Out = O }

    given fieldRemover[ N <: FieldName, Fld <: Field.Named[ N ], Tail <: Tuple ] : FieldRemover.Aux[ N, Fld *: Tail, Tail ] =
        new FieldRemover[ N, Fld *: Tail ] {
            type Out = Tail

            def remove( fields : Fld *: Tail ) : Tail = fields.tail
        }

    given fieldRemoverByIndex[ I <: Int & Singleton, R <: Tuple, Res <: Tuple ](
        using
        fr : Remover.Aux[ I, R, Res ],
    ) : FieldRemover[ I, R ] with {
        type Out = Res

        def remove( fields : R ) : Res = fr.remove( fields )
    }

    def remove[ N <: Singleton, R <: Tuple, Res <: Tuple ](
        fieldName : N,
        from : R,
    )(
        using
        rm : FieldRemover[ N, R ],
    ) : rm.Out = rm.remove( from )
}


trait FieldTypeRemover[ T, N <: Nat, R <: Tuple ] {
    type Out <: Tuple

    def remove( fields : R ) : Out
}

trait LowPriorityFieldTypeRemovers {
    given nextFieldRemover[ T , N <: Nat, Head, Tail <: Tuple, Res <: Tuple ](
        using
        next : FieldTypeRemover.Aux[ T, N, Tail, Res ],
    ) : FieldTypeRemover[ T, N, Head *: Tail ] with {
        type Out = Head *: Res

        override def remove( fields: Head *: Tail ): Head *: Res = fields.head *: next.remove( fields.tail )
    }
}

object FieldTypeRemover extends LowPriorityFieldTypeRemovers {
    type Aux[ T, N <: Nat, R <: Tuple, O <: Tuple ] =
        FieldTypeRemover[ T, N, R ] { type Out = O }

    given fieldRemoverByTypeZero[ T, Fld <: Field.Tpe[ T ], Tail <: Tuple ] : FieldTypeRemover[ T, Nat._0, Fld *: Tail ] with {
        type Out = Tail

        def remove( fields : Fld *: Tail ) : Tail = fields.tail
    }

    given fieldRemoverByTypeNonZero[ T, N <: Nat, DecN <: Nat, Fld <: Field.Tpe[ T ], Tail <: Tuple, Res <: Tuple ](
        using
        ev :  Nat.DecA[ N, DecN ],
        next : FieldTypeRemover.Aux[ T, DecN, Tail, Res ],
    ) : FieldTypeRemover[ T, N, Fld *: Tail ] with {
        type Out = Fld *: Res

        def remove( fields : Fld *: Tail ) : Fld *: Res = fields.head *: next.remove( fields.tail )
    }

}
