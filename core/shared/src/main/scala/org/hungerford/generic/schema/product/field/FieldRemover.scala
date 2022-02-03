package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.types.{Nat, Remover, Replacer, Retriever}

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

    given fieldRemover[ T, F, N <: FieldName, S, Tail <: Tuple ] : FieldRemover.Aux[ N, Field[ T, F, N, S ] *: Tail, Tail ] = new FieldRemover[ N, Field[ T, F, N, S ] *: Tail ] {
        type Out = Tail

        def remove( fields : Field[ T, F, N, S ] *: Tail ) : Tail = fields.tail
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
                ) : Field[ NewT, NewF, NewN, NewS ] *: Tail = withField *: fields.tail
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
