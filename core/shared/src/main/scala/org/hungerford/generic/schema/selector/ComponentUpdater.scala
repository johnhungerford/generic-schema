package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.coproduct.{CoproductSchemaBuilder, CoproductShape, UniqueDiscriminatorValues, UniqueTypeNames, ValidDiscriminator, subtype}
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.{ComplexSchema, Schema, SchemaRebuilder}
import org.hungerford.generic.schema.product.{ProductSchemaBuilder, ProductShape, ValidAfExtr, field}
import org.hungerford.generic.schema.product.field.{Field, FieldBuilder, FieldName, FieldReplacer, FieldRetriever, FieldTypeReplacer, FieldTypeRetriever, UniqueFieldNames}
import org.hungerford.generic.schema.types.{CtxWrapTuplesConstraint, Nat}
import org.hungerford.generic.schema.coproduct.subtype.{CorrectDV, Subtype, SubtypeBuilder, SubtypeReplacer, SubtypeRetriever, TypeName}

import scala.util.NotGiven

trait ComponentUpdater[ Outer, Sel, Inner, NewInner ] {
    type NewOuter

    def update( outer : Outer )( updater : Inner => NewInner ) : NewOuter
}

trait LowPriorityComponentUpdaters {
    given ambigFieldUpdater[ Outer, N <: Singleton, Inner, NewInner, NO ](
        using
        fcu : ComponentUpdater.Aux[ Outer, FieldSelector[ N ], Inner, NewInner, NO ],
    ) : ComponentUpdater.Aux[ Outer, AmbigSelector[ N ], Inner, NewInner, NO ] =
        new ComponentUpdater[ Outer, AmbigSelector[ N ], Inner, NewInner ] {
            type NewOuter = NO

            override def update( outer: Outer )( updater: Inner => NewInner ): NO =
                fcu.update( outer )( updater )
        }

    given ambigSubTypeUpdater[ Outer, N <: Singleton, Inner, NewInner, NO ](
        using
        fcu : ComponentUpdater.Aux[ Outer, SubTypeSelector[ N ], Inner, NewInner, NO ],
    ) : ComponentUpdater.Aux[ Outer, AmbigSelector[ N ], Inner, NewInner, NO ] =
        new ComponentUpdater[ Outer, AmbigSelector[ N ], Inner, NewInner ] {
            type NewOuter = NO

            override def update( outer: Outer )( updater: Inner => NewInner ): NO = fcu.update( outer )( updater )
        }
}

object ComponentUpdater extends LowPriorityComponentUpdaters {
    type Aux[ Outer, Sel, Inner, NewInner, NO ] = ComponentUpdater[ Outer, Sel, Inner, NewInner ] { type NewOuter = NO }

    given [ Outer, NO ] : ComponentUpdater.Aux[ Outer, EmptyTuple, Outer, NO, NO ] =
        new ComponentUpdater[ Outer, EmptyTuple, Outer, NO ] {
            type NewOuter = NO

            def update( outer: Outer )( updater: Outer => NO ): NO = updater( outer )
        }

    given [ Outer, SelHead, SelTail <: Tuple, HeadInner, NewHeadInner, Inner, NewInner, NO ](
        using
        hrt : ComponentRetriever.Aux[ Outer, SelHead, HeadInner ],
        tcu : ComponentUpdater.Aux[ HeadInner, SelTail, Inner, NewInner, NewHeadInner ],
        hcu : => ComponentUpdater.Aux[ Outer, SelHead, HeadInner, NewHeadInner, NO ],
    ) : ComponentUpdater.Aux[ Outer, SelHead *: SelTail, Inner, NewInner, NO ] =
        new ComponentUpdater[ Outer, SelHead *: SelTail, Inner, NewInner ] {
            type NewOuter = NO

            override def update( outer: Outer )( updater: Inner => NewInner ): NO = {
                hcu.update( outer )( ( i: HeadInner ) => tcu.update( i )( updater ) )
            }
        }

    given fromProductSchema[ SelN <: Singleton, T, R <: Tuple, NewR <: Tuple, RV <: Tuple, AF, AFS, AFE, C, N <: FieldName, F, FS, NewN <: FieldName, NewFS ](
        using
        frt : FieldRetriever.Aux[ N, R, Field[ T, F, N, FS ] ],
        frp : FieldReplacer.Aux[ N, R, T, F, NewN, NewFS, NewR ],
        unq : UniqueFieldNames[ NewR ],
        ctx : CtxWrapTuplesConstraint[ Field.Of, NewR, RV ],
        pc : ProductConstructor[ C, RV, AF, T ],
        pdc : ProductDeconstructor[ T, (AF, R) ],
        afeEv : ValidAfExtr[ T, AF, AFE ],
    ) : ComponentUpdater.Aux[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ], FieldSelector[ SelN ], Field[ T, F, N, FS ], Field[ T, F, NewN, NewFS ], Schema.Aux[ T, ProductShape[ T, NewR, RV, AF, AFS, AFE, C ] ] ] = {
        new ComponentUpdater[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ], FieldSelector[ SelN ], Field[ T, F, N, FS ], Field[ T, F, NewN, NewFS ] ] {
            override type NewOuter = Schema.Aux[ T, ProductShape[ T, NewR, RV, AF, AFS, AFE, C ] ]

            override def update(
                outer : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            )(
                updater : Field[ T, F, N, FS ] => Field[ T, F, NewN, NewFS ],
            ) : Schema.Aux[ T, ProductShape[ T, NewR, RV, AF, AFS, AFE, C ] ] = {
                val field = frt.retrieve( outer.shape.fieldDescriptions )
                val newField = updater( field )
                ComplexSchema[ T, ProductShape[ T, NewR, RV, AF, AFS, AFE, C ] ](
                    genericDescription = outer.genericDescription,
                    genericValidators = outer.genericValidators,
                    shape = outer.shape.copy[ T, NewR, RV, AF, AFS, AFE, C ](
                        fieldDescriptions = frp.replace( outer.shape.fieldDescriptions, newField )
                    )
                )
            }
        }
    }

    given fromProductSchemaUsingType[ F, SelN <: Nat, T, R <: Tuple, NewR <: Tuple, RV <: Tuple, AF, AFS, AFE, C, NewN <: FieldName, NewFS, Fld <: Field.Extr[ T, F ] ](
        using
        frt : FieldTypeRetriever.Aux[ F, SelN, R, Fld ],
        frp : FieldTypeReplacer.Aux[ F, SelN, R, T, F, NewN, NewFS, NewR ],
        unq : UniqueFieldNames[ NewR ],
        ctx : CtxWrapTuplesConstraint[ Field.Of, NewR, RV ],
        pc : ProductConstructor[ C, RV, AF, T ],
        pdc : ProductDeconstructor[ T, (AF, R) ],
        afeEv : ValidAfExtr[ T, AF, AFE ],
    ) : ComponentUpdater.Aux[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ], FieldSelector[ TypeSelector[ F, SelN ] ], Fld, Field[ T, F, NewN, NewFS ], Schema.Aux[ T, ProductShape[ T, NewR, RV, AF, AFS, AFE, C ] ] ] = {
        new ComponentUpdater[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ], FieldSelector[ TypeSelector[ F, SelN ] ], Fld, Field[ T, F, NewN, NewFS ] ] {
            override type NewOuter = Schema.Aux[ T, ProductShape[ T, NewR, RV, AF, AFS, AFE, C ] ]

            override def update(
                outer : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            )(
                updater : Fld => Field[ T, F, NewN, NewFS ],
            ) : Schema.Aux[ T, ProductShape[ T, NewR, RV, AF, AFS, AFE, C ] ] = {
                val field = frt.retrieve( outer.shape.fieldDescriptions )
                val newField = updater( field )
                ComplexSchema[ T, ProductShape[ T, NewR, RV, AF, AFS, AFE, C ] ](
                    genericDescription = outer.genericDescription,
                    genericValidators = outer.genericValidators,
                    shape = outer.shape.copy[ T, NewR, RV, AF, AFS, AFE, C ](
                        fieldDescriptions = frp.replace( outer.shape.fieldDescriptions, newField )
                        )
                    )
            }
        }
    }

    given fromProductSchemaBuilder[ SelN <: Singleton, T, R <: Tuple, NewR <: Tuple, RV <: Tuple, AF, AFS, AFE, C, N <: FieldName, F, FS, NewN <: FieldName, NewFS ](
        using
        frt : FieldRetriever.Aux[ N, R, Field[ T, F, N, FS ] ],
        frp : FieldReplacer.Aux[ N, R, T, F, NewN, NewFS, NewR ],
        ctx : CtxWrapTuplesConstraint[ Field.Of, NewR, RV ],
    ) : ComponentUpdater.Aux[ ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ], FieldSelector[ SelN ], Field[ T, F, N, FS ], Field[ T, F, NewN, NewFS ], ProductSchemaBuilder[ T, NewR, RV, AF, AFS, AFE, C ] ] = {
        new ComponentUpdater[ ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ], FieldSelector[ SelN ], Field[ T, F, N, FS ], Field[ T, F, NewN, NewFS ] ] {
            override type NewOuter = ProductSchemaBuilder[ T, NewR, RV, AF, AFS, AFE, C ]

            override def update(
                outer : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ],
            )(
                updater : Field[ T, F, N, FS ] => Field[ T, F, NewN, NewFS ],
            ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS, AFE, C ] = {
                val field = frt.retrieve( outer.fieldDescs )
                val newField = updater( field )
                val newFields = frp.replace( outer.fieldDescs, newField )
                outer.copy[ T, NewR, RV, AF, AFS, AFE, C ]( fieldDescs = newFields )
            }
        }
    }

    given fromProductSchemaBuilderUsingType[ F, SelN <: Nat, T, R <: Tuple, NewR <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Fld <: Field.Extr[ T, F ], NewN <: FieldName, NewFS ](
        using
        frt : FieldTypeRetriever.Aux[ F, SelN, R, Fld ],
        frp : FieldTypeReplacer.Aux[ F, SelN, R, T, F, NewN, NewFS, NewR ],
        ctx : CtxWrapTuplesConstraint[ Field.Of, NewR, RV ],
    ) : ComponentUpdater.Aux[ ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ], FieldSelector[ TypeSelector[ F, SelN ] ], Fld, Field[ T, F, NewN, NewFS ], ProductSchemaBuilder[ T, NewR, RV, AF, AFS, AFE, C ] ] = {
        new ComponentUpdater[ ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ], FieldSelector[ TypeSelector[ F, SelN ] ], Fld, Field[ T, F, NewN, NewFS ] ] {
            override type NewOuter = ProductSchemaBuilder[ T, NewR, RV, AF, AFS, AFE, C ]

            override def update(
                outer : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ],
            )(
                updater : Fld => Field[ T, F, NewN, NewFS ],
            ) : ProductSchemaBuilder[ T, NewR, RV, AF, AFS, AFE, C ] = {
                val field = frt.retrieve( outer.fieldDescs )
                val newField = updater( field )
                val newFields = frp.replace( outer.fieldDescs, newField )
                outer.copy[ T, NewR, RV, AF, AFS, AFE, C ]( fieldDescs = newFields )
            }
        }
    }

    given fromFieldDesc[ T, F, N <: FieldName, S, SelR, I, NewI, NewS ](
        using
        ev : NotGiven[ SelR <:< Tuple ],
        scu : ComponentUpdater.Aux[ Schema.Aux[ F, S ], SelR, I, NewI, Schema.Aux[ F, NewS ] ],
    ) : ComponentUpdater.Aux[ Field[ T, F, N, S ], SelR, I, NewI, Field[ T, F, N, NewS ] ] = {
        new ComponentUpdater[ Field[ T, F, N, S ], SelR, I, NewI ] {
            override type NewOuter = Field[ T, F, N, NewS ]

            override def update( outer : Field[ T, F, N, S ] )( updater : I => NewI ) : Field[ T, F, N, NewS ] =
                FieldBuilder.from( outer ).fromSchema( scu.update( outer.schema )( updater ) ).build
        }
    }

    given fromCoproductSchema[ SelN <: Singleton, T, R <: Tuple, RV <: Tuple, D, DN, N <: TypeName, ST, DV, STS, NewDV, NewN <: TypeName, NewSTS, NewR <: Tuple, NewRV <: Tuple ](
        using
        strt : SubtypeRetriever.Aux[ SelN, R, Subtype.Aux[ T, ST, D, DN, DV, N, STS ] ],
        strp : SubtypeReplacer.Aux[ SelN, Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ], R, NewR ],
        ctx : CtxWrapTuplesConstraint[ Subtype.Ctx[ T, D ], NewR, NewRV ],
        uniqT : UniqueTypeNames[ NewR ],
        uniqDV : UniqueDiscriminatorValues[ NewR ],
        dEv : ValidDiscriminator[ D, DN, NewR ],
    ) : ComponentUpdater.Aux[ Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ], SubTypeSelector[ SelN ], Subtype.Aux[ T, ST, D, DN, DV, N, STS ], Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ], Schema.Aux[ T, CoproductShape[ T, NewR, NewRV, D, DN ] ] ] = {
        new ComponentUpdater[ Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ], SubTypeSelector[ SelN ], Subtype.Aux[ T, ST, D, DN, DV, N, STS ], Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ] ] {
            type NewOuter = Schema.Aux[ T, CoproductShape[ T, NewR, NewRV, D, DN ] ]

            def update(
                outer : Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            )(
                updater : Subtype.Aux[ T, ST, D, DN, DV, N, STS ] => Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ],
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

    given fromCoproductSchemaBuilder[ SelN <: Singleton, T, R <: Tuple, D, DN, N <: TypeName, ST, DV, STS, NewDV, NewN <: TypeName, NewSTS, NewR <: Tuple ](
        using
        strt : SubtypeRetriever.Aux[ SelN, R, Subtype.Aux[ T, ST, D, DN, DV, N, STS ] ],
        strp : SubtypeReplacer.Aux[ SelN, Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ], R, NewR ],
        uniqT : UniqueTypeNames[ NewR ],
        uniqDV : UniqueDiscriminatorValues[ NewR ],
        dEv : ValidDiscriminator[ D, DN, NewR ],
    ) : ComponentUpdater.Aux[ CoproductSchemaBuilder[ T, R, D, DN ], SubTypeSelector[ SelN ], Subtype.Aux[ T, ST, D, DN, DV, N, STS ], Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ], CoproductSchemaBuilder[ T, NewR, D, DN ] ] = {
        new ComponentUpdater[ CoproductSchemaBuilder[ T, R, D, DN ], SubTypeSelector[ SelN ], Subtype.Aux[ T, ST, D, DN, DV, N, STS ], Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ] ] {
            type NewOuter = CoproductSchemaBuilder[ T, NewR, D, DN ]

            def update(
                outer : CoproductSchemaBuilder[ T, R, D, DN ],
            )(
                updater : Subtype.Aux[ T, ST, D, DN, DV, N, STS ] => Subtype.Aux[ T, ST, D, DN, NewDV, NewN, NewSTS ],
            ) : NewOuter = {
                val inner = strt.retrieve( outer.sts )
                val newInner = updater( inner )
                val newSts = strp.replace( outer.sts, newInner )
                outer.copy[ T, NewR, D, DN ]( sts = newSts )
            }
        }
    }

    given fromSubtype[ T, ST, D, DN, DV, N <: FieldName, S, SelR, I, NewI, NewS ](
        using
        ev : NotGiven[ SelR <:< Tuple ],
        ev1 : CorrectDV[ D, DV ],
        ev2 : ValidDiscriminator[ D, DN, Subtype.Aux[ T, ST, D, DN, DV, N, NewS ] *: EmptyTuple ],
        scu : ComponentUpdater.Aux[ Schema.Aux[ ST, S ], SelR, I, NewI, Schema.Aux[ ST, NewS ] ],
    ) : ComponentUpdater.Aux[ Subtype.Aux[ T, ST, D, DN, DV, N, S ], SelR, I, NewI, Subtype.Aux[ T, ST, D, DN, DV, N, NewS ] ] = {
        new ComponentUpdater[ Subtype.Aux[ T, ST, D, DN, DV, N, S ], SelR, I, NewI ] {
            override type NewOuter = Subtype.Aux[ T, ST, D, DN, DV, N, NewS ]

            override def update( outer : Subtype.Aux[ T, ST, D, DN, DV, N, S ] )( updater : I => NewI ) : NewOuter =
                SubtypeBuilder.from( outer ).fromSchema( scu.update( outer.schema )( updater ) ).build
        }
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
