package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.{Schema, SchemaBuilder, SchemaRebuilder}
import org.hungerford.generic.schema.product.{CtxWrapTuplesConstraint, ProductSchemaBuilder, ProductShape, TupleIntLength}
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder, FieldDescriptionCase, FieldName, FieldReplacer, FieldRetriever, UniqueFieldNames}

trait ComponentUpdater[ Outer, Sel, Inner, NewInner ] {
    type NewOuter

    def update( outer : Outer )( updater : Inner => NewInner ) : NewOuter
}

object ComponentUpdater {
    type Aux[ Outer, Sel, Inner, NewInner, NO ] = ComponentUpdater[ Outer, Sel, Inner, NewInner ] { type NewOuter = NO }

    given [ Outer, NO ] : ComponentUpdater.Aux[ Outer, EmptyTuple, Outer, NO, NO ] = new ComponentUpdater[ Outer, EmptyTuple, Outer, NO ] {
        type NewOuter = NO

        def update( outer : Outer )( updater : Outer => NO ) : NO = updater( outer )
    }

    given [ Outer, SelHead, SelTail <: Tuple, HeadInner, NewHeadInner, Inner, NewInner, NO ](
        using
        hcu : ComponentUpdater.Aux[ Outer, SelHead, HeadInner, NewHeadInner, NO ],
        tcu : ComponentUpdater.Aux[ HeadInner, SelTail, Inner, NewInner, NewHeadInner ],
    ) : ComponentUpdater.Aux[ Outer, SelHead *: SelTail, Inner, NewInner, NO ] = new ComponentUpdater[ Outer, SelHead *: SelTail, Inner, NewInner ] {
        type NewOuter = NO

        override def update( outer : Outer )( updater : Inner => NewInner ) : NO = {
            hcu.update( outer )( ( i : HeadInner ) => tcu.update( i )( updater ) )
        }
    }

    given [ T, R <: Tuple, NewR <: Tuple, RV <: Tuple, AF, AFS, C, DC, N <: FieldName, F, FS, NewN <: FieldName, NewFS ](
        using
        cr : ComponentRetriever.Aux[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ], FieldSelector[ N ], FieldDescription.Aux[ F, N, FS ] ],
        frt : FieldRetriever.Aux[ N, R, FieldDescription.Aux[ F, N, FS ] ],
        frp : FieldReplacer.Aux[ N, R, F, NewN, NewFS, NewR ],
        ctx : CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
        rb : SchemaRebuilder.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ], ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] ],
        il : TupleIntLength[ NewR ],
        unq : UniqueFieldNames[ NewR ],
        pc : ProductConstructor[ C, RV, AF, T ],
        pdc : ProductDeconstructor[ DC, RV, AF, T ],
    ) : ComponentUpdater.Aux[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ], FieldSelector[ N ], FieldDescription.Aux[ F, N, FS ], FieldDescription.Aux[ F, NewN, NewFS ], Schema.Aux[ T, ProductShape[ T, NewR, RV, AF, AFS, C, DC ] ] ] = {
        new ComponentUpdater[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ], FieldSelector[ N ], FieldDescription.Aux[ F, N, FS ], FieldDescription.Aux[ F, NewN, NewFS ] ] {
            override type NewOuter = Schema.Aux[ T, ProductShape[ T, NewR, RV, AF, AFS, C, DC ] ]

            override def update(
                outer : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ],
            )(
                updater : FieldDescription.Aux[ F, N, FS ] => FieldDescription.Aux[ F, NewN, NewFS ],
            ) : Schema.Aux[ T, ProductShape[ T, NewR, RV, AF, AFS, C, DC ] ] = {
                val field = cr.retrieve( outer )
                SchemaBuilder.from( outer ).updateField( field.fieldName )( using frt )( v => updater( v.build ) )( using frp, ctx ).build
            }
        }
    }

    given [ T, N <: FieldName, S, SelN <: FieldName, Inner, NewInner, NewS ](
        using
        scu : ComponentUpdater.Aux[ Schema.Aux[ T, S ], FieldSelector[ SelN ], Inner, NewInner, Schema.Aux[ T, NewS ] ],
    ) : ComponentUpdater.Aux[ FieldDescription.Aux[ T, N, S ], FieldSelector[ SelN ], Inner, NewInner, FieldDescription.Aux[ T, N, NewS ] ] = {
        new ComponentUpdater[ FieldDescription.Aux[ T, N, S ], FieldSelector[ SelN ], Inner, NewInner ] {
            override type NewOuter = FieldDescription.Aux[ T, N, NewS ]

            override def update( outer : FieldDescription.Aux[ T, N, S ] )( updater : Inner => NewInner ) : FieldDescription.Aux[ T, N, NewS ] =
                FieldDescriptionBuilder.from( outer ).fromSchema( scu.update( outer.schema )( updater ) ).build
        }
    }

    given fieldAmbig[ Outer, N <: FieldName, Inner, NewInner, NO ](
        using
        fcu : ComponentUpdater.Aux[ Outer, FieldSelector[ N ], Inner, NewInner, NO ],
    ) : ComponentUpdater.Aux[ Outer, AmbigSelector[ N ], Inner, NewInner, NO ] = new ComponentUpdater[ Outer, AmbigSelector[ N ], Inner, NewInner ] {
        override type NewOuter = fcu.NewOuter

        override def update( outer : Outer )( updater : Inner => NewInner ) : fcu.NewOuter = fcu.update( outer )( updater )
    }

    given subtypeAmbig[ Outer, N <: FieldName, Inner, NewInner, NO ](
        using
        stcu : ComponentUpdater.Aux[ Outer, SubTypeSelector[ N ], Inner, NewInner, NO ],
    ) : ComponentUpdater.Aux[ Outer, AmbigSelector[ N ], Inner, NewInner, NO ] = new ComponentUpdater[ Outer, AmbigSelector[ N ], Inner, NewInner ] {
        override type NewOuter = stcu.NewOuter

        override def update( outer : Outer )( updater : Inner => NewInner ) : stcu.NewOuter = stcu.update( outer )( updater )
    }


    class Updater[ Outer, Sel, Inner ](
        outer : Outer,
    )(
        using
        cr : ComponentRetriever.Aux[ Outer, Sel, Inner ],
    ) {
        def apply[ NewInner ](
            updater : Inner => NewInner,
        )(
            using
            cu : ComponentUpdater[ Outer, Sel, Inner, NewInner ],
        ) : cu.NewOuter = cu.update( outer )( updater )
    }

    def update[ T, S, R <: Tuple, F, N <: FieldName, FS ](
        outer : Schema.Aux[ T, S ],
    )(
        sel : Selector[ R ],
    )(
        using
        cr : ComponentRetriever.Aux[ Schema.Aux[ T, S ], R, FieldDescription.Aux[ F, N, FS ] ],
    ) : Updater[ Schema.Aux[ T, S ], R, FieldDescription.Aux[ F, N, FS ] ] = new Updater[ Schema.Aux[ T, S ], R, FieldDescription.Aux[ F, N, FS ] ]( outer )

}
