package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.{ComplexSchema, Schema, SchemaBuilder, SchemaRebuilder}
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
        hrt : ComponentRetriever.Aux[ Outer, SelHead, HeadInner ],
        tcu : ComponentUpdater.Aux[ HeadInner, SelTail, Inner, NewInner, NewHeadInner ],
        hcu : => ComponentUpdater.Aux[ Outer, SelHead, HeadInner, NewHeadInner, NO ],
    ) : ComponentUpdater.Aux[ Outer, SelHead *: SelTail, Inner, NewInner, NO ] = new ComponentUpdater[ Outer, SelHead *: SelTail, Inner, NewInner ] {
        type NewOuter = NO

        override def update( outer : Outer )( updater : Inner => NewInner ) : NO = {
            hcu.update( outer )( ( i : HeadInner ) => tcu.update( i )( updater ) )
        }
    }

    given fromSchema[ T, R <: Tuple, NewR <: Tuple, RV <: Tuple, AF, AFS, C, DC, N <: FieldName, F, FS, NewN <: FieldName, NewFS ](
        using
        frt : FieldRetriever.Aux[ N, R, FieldDescription.Aux[ F, N, FS ] ],
        frp : FieldReplacer.Aux[ N, R, F, NewN, NewFS, NewR ],
        unq : UniqueFieldNames[ NewR ],
        ctx : CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
        il : TupleIntLength[ NewR ],
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
                val field = frt.retrieve( outer.shape.fieldDescriptions )
                val newField = updater( field )
                ComplexSchema[ T, ProductShape[ T, NewR, RV, AF, AFS, C, DC ] ](
                    genericDescription = outer.genericDescription,
                    genericValidators = outer.genericValidators,
                    shape = outer.shape.copy[ T, NewR, RV, AF, AFS, C, DC ](
                        fieldDescriptions = frp.replace( outer.shape.fieldDescriptions, newField )
                    )
                )
            }
        }
    }

    given fromProductSchemaBuilder[ T, R <: Tuple, NewR <: Tuple, RV <: Tuple, AF, AFS, C, DC, N <: FieldName, F, FS, NewN <: FieldName, NewFS ](
        using
        frt : FieldRetriever.Aux[ N, R, FieldDescription.Aux[ F, N, FS ] ],
        frp : FieldReplacer.Aux[ N, R, F, NewN, NewFS, NewR ],
        ctx : CtxWrapTuplesConstraint[ FieldDescription, NewR, RV ],
    ) : ComponentUpdater.Aux[ ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ], FieldSelector[ N ], FieldDescription.Aux[ F, N, FS ], FieldDescription.Aux[ F, NewN, NewFS ], ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ] ] = {
        new ComponentUpdater[ ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ], FieldSelector[ N ], FieldDescription.Aux[ F, N, FS ], FieldDescription.Aux[ F, NewN, NewFS ] ] {
            override type NewOuter = ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ]

            override def update(
                outer : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ],
            )(
                updater : FieldDescription.Aux[ F, N, FS ] => FieldDescription.Aux[ F, NewN, NewFS ],
            ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ] = {
                val field = frt.retrieve( outer.fieldDescs )
                val newField = updater( field )
                val newFields = frp.replace( outer.fieldDescs, newField )
                outer.copy[ T, NewR, RV, AF, AFS, C, DC ]( fieldDescs = newFields )
            }
        }
    }

    given fromFieldDesc[ T, N <: FieldName, S, SelN <: FieldName, F, FS, NewN <: FieldName, NewFS, NewS ](
        using
        scu : ComponentUpdater.Aux[ Schema.Aux[ T, S ], FieldSelector[ SelN ], FieldDescription.Aux[ F, SelN, FS ], FieldDescription.Aux[ F, NewN, NewFS ], Schema.Aux[ T, NewS ] ],
    ) : ComponentUpdater.Aux[ FieldDescription.Aux[ T, N, S ], FieldSelector[ SelN ], FieldDescription.Aux[ F, SelN, FS ], FieldDescription.Aux[ F, NewN, NewFS ], FieldDescription.Aux[ T, N, NewS ] ] = {
        new ComponentUpdater[ FieldDescription.Aux[ T, N, S ], FieldSelector[ SelN ], FieldDescription.Aux[ F, SelN, FS ], FieldDescription.Aux[ F, NewN, NewFS ] ] {
            override type NewOuter = FieldDescription.Aux[ T, N, NewS ]

            override def update( outer : FieldDescription.Aux[ T, N, S ] )( updater : FieldDescription.Aux[ F, SelN, FS ] => FieldDescription.Aux[ F, NewN, NewFS ] ) : FieldDescription.Aux[ T, N, NewS ] =
                FieldDescriptionBuilder.from( outer ).fromSchema( scu.update( outer.schema )( updater ) ).build
        }
    }

    given ambigFieldUpdater[ Outer, N <: FieldName, Inner, NewInner, NO ](
        using
        fcu : ComponentUpdater.Aux[ Outer, FieldSelector[ N ], Inner, NewInner, NO ],
    ) : ComponentUpdater.Aux[ Outer, AmbigSelector[ N ], Inner, NewInner, NO ] = new ComponentUpdater[ Outer, AmbigSelector[ N ], Inner, NewInner ] {
        type NewOuter = NO

        override def update( outer : Outer )( updater : Inner => NewInner ) : NO = fcu.update( outer )( updater )
    }

    given ambigSubTypeUpdater[ Outer, N <: FieldName, Inner, NewInner, NO ](
        using
        fcu : ComponentUpdater.Aux[ Outer, SubTypeSelector[ N ], Inner, NewInner, NO ],
    ) : ComponentUpdater.Aux[ Outer, AmbigSelector[ N ], Inner, NewInner, NO ] = new ComponentUpdater[ Outer, AmbigSelector[ N ], Inner, NewInner ] {
        type NewOuter = NO

        override def update( outer : Outer )( updater : Inner => NewInner ) : NO = fcu.update( outer )( updater )
    }


    class Updater[ T, S, Sel, F, N <: FieldName, FS ](
        outer : Schema.Aux[ T, S ],
    )(
        using
        cr : ComponentRetriever.Aux[ Schema.Aux[ T, S ], Sel, FieldDescription.Aux[ F, N, FS ] ],
    ) {
        def apply[ NewN <: FieldName, NewFS ](
            updater : FieldDescription.Aux[ F, N, FS ] => FieldDescription.Aux[ F, NewN, NewFS ],
        )(
            using
            cu : ComponentUpdater[ Schema.Aux[ T, S ], Sel, FieldDescription.Aux[ F, N, FS ], FieldDescription.Aux[ F, NewN, NewFS ] ],
        ) : cu.NewOuter = cu.update( outer )( updater )
    }

    def update[ T, S, R <: Tuple, F, N <: FieldName, FS ](
        outer : Schema.Aux[ T, S ],
    )(
        sel : Selector[ R ],
    )(
        using
        cr : ComponentRetriever.Aux[ Schema.Aux[ T, S ], R, FieldDescription.Aux[ F, N, FS ] ],
    ) : Updater[ T, S, R, F, N, FS ] = new Updater[ T, S, R, F, N, FS ]( outer )

}
