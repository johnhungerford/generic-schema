package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.coproduct.{CoproductShape, UniqueDiscriminatorValues, UniqueTypeNames, ValidDiscriminator}
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.{ComplexSchema, Schema, SchemaRebuilder}
import org.hungerford.generic.schema.product.{ProductSchemaBuilder, ProductShape, TupleIntLength}
import org.hungerford.generic.schema.product.field.{Field, FieldBuilder, FieldCase, FieldName, FieldReplacer, FieldRetriever, UniqueFieldNames}
import org.hungerford.generic.schema.types.CtxWrapTuplesConstraint
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, SubtypeReplacer, SubtypeRetriever, TypeName}

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

    given fromProductSchema[ T, R <: Tuple, NewR <: Tuple, RV <: Tuple, AF, AFS, C, DC, N <: FieldName, F, FS, NewN <: FieldName, NewFS ](
        using
        frt : FieldRetriever.Aux[ N, R, Field.Aux[ F, N, FS ] ],
        frp : FieldReplacer.Aux[ N, R, F, NewN, NewFS, NewR ],
        unq : UniqueFieldNames[ NewR ],
        ctx : CtxWrapTuplesConstraint[ Field, NewR, RV ],
        il : TupleIntLength[ NewR ],
        pc : ProductConstructor[ C, RV, AF, T ],
        pdc : ProductDeconstructor[ DC, RV, AF, T ],
    ) : ComponentUpdater.Aux[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ], FieldSelector[ N ], Field.Aux[ F, N, FS ], Field.Aux[ F, NewN, NewFS ], Schema.Aux[ T, ProductShape[ T, NewR, RV, AF, AFS, C, DC ] ] ] = {
        new ComponentUpdater[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ], FieldSelector[ N ], Field.Aux[ F, N, FS ], Field.Aux[ F, NewN, NewFS ] ] {
            override type NewOuter = Schema.Aux[ T, ProductShape[ T, NewR, RV, AF, AFS, C, DC ] ]

            override def update(
                outer : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ],
            )(
                updater : Field.Aux[ F, N, FS ] => Field.Aux[ F, NewN, NewFS ],
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
        frt : FieldRetriever.Aux[ N, R, Field.Aux[ F, N, FS ] ],
        frp : FieldReplacer.Aux[ N, R, F, NewN, NewFS, NewR ],
        ctx : CtxWrapTuplesConstraint[ Field, NewR, RV ],
    ) : ComponentUpdater.Aux[ ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ], FieldSelector[ N ], Field.Aux[ F, N, FS ], Field.Aux[ F, NewN, NewFS ], ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ] ] = {
        new ComponentUpdater[ ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ], FieldSelector[ N ], Field.Aux[ F, N, FS ], Field.Aux[ F, NewN, NewFS ] ] {
            override type NewOuter = ProductSchemaBuilder[ T, NewR, RV, AF, AFS, C, DC ]

            override def update(
                outer : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ],
            )(
                updater : Field.Aux[ F, N, FS ] => Field.Aux[ F, NewN, NewFS ],
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
        scu : ComponentUpdater.Aux[ Schema.Aux[ T, S ], FieldSelector[ SelN ], Field.Aux[ F, SelN, FS ], Field.Aux[ F, NewN, NewFS ], Schema.Aux[ T, NewS ] ],
    ) : ComponentUpdater.Aux[ Field.Aux[ T, N, S ], FieldSelector[ SelN ], Field.Aux[ F, SelN, FS ], Field.Aux[ F, NewN, NewFS ], Field.Aux[ T, N, NewS ] ] = {
        new ComponentUpdater[ Field.Aux[ T, N, S ], FieldSelector[ SelN ], Field.Aux[ F, SelN, FS ], Field.Aux[ F, NewN, NewFS ] ] {
            override type NewOuter = Field.Aux[ T, N, NewS ]

            override def update( outer : Field.Aux[ T, N, S ] )( updater : Field.Aux[ F, SelN, FS ] => Field.Aux[ F, NewN, NewFS ] ) : Field.Aux[ T, N, NewS ] =
                FieldBuilder.from( outer ).fromSchema( scu.update( outer.schema )( updater ) ).build
        }
    }

    given fromCoproductSchema[ T, R <: Tuple, RV <: Tuple, D, DN, SelN <: TypeName, ST, DV, STS, NewDV, NewN <: TypeName, NewSTS, NewR <: Tuple, NewRV <: Tuple ](
        using
        strt : SubtypeRetriever.Aux[ SelN, R, Subtype.Aux[ T, ST, D, DN, DV, SelN, STS ] ],
        strp : SubtypeReplacer.Aux[ SelN, Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ], R, NewR ],
        ctx : CtxWrapTuplesConstraint[ Subtype.Ctx[ T, D ], NewR, NewRV ],
        uniqT : UniqueTypeNames[ NewR ],
        uniqDV : UniqueDiscriminatorValues[ NewR ],
        dEv : ValidDiscriminator[ D, DN, NewR ],
    ) : ComponentUpdater.Aux[ Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ], SubTypeSelector[ SelN ], Subtype.Aux[ T, ST, D, DN, DV, SelN, STS ], Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ], Schema.Aux[ T, CoproductShape[ T, NewR, NewRV, D, DN ] ] ] = {
        new ComponentUpdater[ Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ], SubTypeSelector[ SelN ], Subtype.Aux[ T, ST, D, DN, DV, SelN, STS ], Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ] ] {
            type NewOuter = Schema.Aux[ T, CoproductShape[ T, NewR, NewRV, D, DN ] ]

            def update(
                outer : Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            )(
                updater : Subtype.Aux[ T, ST, D, DN, DV, SelN, STS ] => Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ],
            ) : NewOuter = {
                val inner = strt.retrieve( outer.shape.subtypeDescriptions )
                val newInner = updater( inner )
                ComplexSchema[ T, CoproductShape[ T, NewR, NewRV, D, DN ] ](
                    outer.shape.copy[ T, NewR, NewRV, D, DN ]( subtypeDescriptions = strp.replace( outer.shape.subtypeDescriptions, newInner ) ),
                    name = outer.name,
                    genericDescription = outer.genericDescription,
                    genericValidators = outer.genericValidators,
                    genericExamples = outer.genericExamples,
                    deprecated = outer.deprecated,
                )
            }
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


    class Updater[ Outer, Inner, Sel ](
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

    def update[ Outer, Inner, R <: Tuple ](
        outer : Outer,
    )(
        sel : Selector[ R ],
    )(
        using
        cr : ComponentRetriever.Aux[ Outer, R, Inner ],
    ) : Updater[ Outer, Inner, R ] = new Updater[ Outer, Inner, R ]( outer )

}
