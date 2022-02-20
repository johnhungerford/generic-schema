package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.product.field.LazyField
import org.hungerford.generic.schema.types.{Nat, Retriever}

trait FieldRetriever[ N <: Singleton, R <: Tuple ] {
    type Fld

    def retrieve( from : R ) : Fld
}

trait LowPriorityFieldRetrievers {
    given [ N <: Singleton, Head, Tail <: Tuple, Next ](
        using
        next : FieldRetriever.Aux[ N, Tail, Next ],
    ) : FieldRetriever.Aux[ N, Head *: Tail, Next ] = {
        new FieldRetriever[ N, Head *: Tail ] {
            type Fld = next.Fld

            override def retrieve( from : Head *: Tail ) : Next =
                next.retrieve( from.tail )
        }
    }
}

object FieldRetriever extends LowPriorityFieldRetrievers {
    type Aux[ N <: Singleton, R <: Tuple, F ] =
        FieldRetriever[ N, R ] { type Fld = F }

    given [ N <: FieldName, T, F, S, Tail <: Tuple ] :
    FieldRetriever.Aux[ N, Field[ T, F, N, S ] *: Tail, Field[ T, F, N, S ] ] = {
        new FieldRetriever[ N, Field[ T, F, N, S ] *: Tail ] {
            type Fld = Field[ T, F, N, S ]

            override def retrieve( from : Field[ T, F, N, S ] *: Tail ) : Field[ T, F, N, S ] =
                from.head
        }
    }

    given lz[ N <: FieldName, T, F, Tail <: Tuple ] :
    FieldRetriever.Aux[ N, LazyField[ T, F, N ] *: Tail, LazyField[ T, F, N ] ] = {
        new FieldRetriever[ N, LazyField[ T, F, N ] *: Tail ] {
            type Fld = LazyField[ T, F, N ]

            override def retrieve( from : LazyField[ T, F, N ] *: Tail ) : LazyField[ T, F, N ] =
                from.head
        }
    }

    given fieldRetrieverByIndex[ I <: Int & Singleton, N <: Nat, R <: Tuple, T, F, Nm <: FieldName, S ](
        using
        ev : Nat.IntA[ I, N ],
        rt : Retriever.Aux[ N, R, Field[ T, F, Nm, S ] ],
    ) : FieldRetriever[ I, R ] with {
        type Fld = Field[ T, F, Nm, S ]

        override def retrieve( from : R ) : Field[ T, F, Nm, S ] =
            rt.retrieve( from )
    }

    def retrieve[ N <: Singleton, R <: Tuple ](
        fieldName : N,
        fields : R,
    )(
        using
        fr : FieldRetriever[ N, R ],
    ) : fr.Fld = fr.retrieve( fields )
}

trait FieldTypeRetriever[ T, N <: Nat, R <: Tuple ] {
    type Fld

    def retrieve( from : R ) : Fld
}

trait LowPriorityFieldTypeRetrievers {
    given [ T, N <: Nat, Head, Tail <: Tuple, Next ](
        using
        next : FieldTypeRetriever.Aux[ T, N, Tail, Next ],
    ) : FieldTypeRetriever.Aux[ T, N, Head *: Tail, Next ] = {
        new FieldTypeRetriever[ T, N, Head *: Tail ] {
            type Fld = Next

            override def retrieve( from : Head *: Tail ) : Next =
                next.retrieve( from.tail )
        }
    }
}

object FieldTypeRetriever extends LowPriorityFieldTypeRetrievers {
    type Aux[ T, N <: Nat, R <: Tuple, F ] =
        FieldTypeRetriever[ T, N, R ] { type Fld = F }

    given [ T, Fl <: Field.Of[ T ], Tail <: Tuple ] : FieldTypeRetriever[ T, Nat._0, Fl *: Tail] with {
        type Fld = Fl

        override def retrieve( from : Fl *: Tail ) : Fl =
            from.head
    }

    given [ T, N <: Nat, DecN <: Nat, Fl <: Field.Of[ T ], Tail <: Tuple, Res ](
        using
        ev : Nat.DecA[ N, DecN ],
        next : FieldTypeRetriever.Aux[ T, DecN, Tail, Res ]
    ) : FieldTypeRetriever[ T, N, Fl *: Tail ] with {
        type Fld = Res

        override def retrieve( from: Fl *: Tail ) : Res = next.retrieve( from.tail )
    }

    given fieldRetrieverByIndex[ I <: Int & Singleton, N <: Nat, R <: Tuple, T, F, Nm <: FieldName, S ](
        using
        ev : Nat.IntA[ I, N ],
        rt : Retriever.Aux[ N, R, Field[ T, F, Nm, S ] ],
    ) : FieldTypeRetriever[ I, N, R ] with {
        type Fld = Field[ T, F, Nm, S ]

        override def retrieve( from : R ) : Field[ T, F, Nm, S ] =
            rt.retrieve( from )
    }

}

