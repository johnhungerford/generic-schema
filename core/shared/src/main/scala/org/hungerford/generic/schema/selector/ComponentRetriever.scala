package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.{Schema, selector}
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, LazySubtype, SubtypeRetriever, SubtypeTypeRetriever, TypeName}
import org.hungerford.generic.schema.coproduct.{CoproductSchemaBuilder, CoproductShape, subtype}
import org.hungerford.generic.schema.product.field.{Field, FieldName, FieldReplacer, FieldRetriever, FieldTypeRetriever, LazyField}
import org.hungerford.generic.schema.product.{ProductSchemaBuilder, ProductShape}
import org.hungerford.generic.schema.types.Nat

import scala.util.NotGiven

trait ComponentRetriever[ Outer, Sel ] {
    type Inner

    def retrieve( from : Outer ) : Inner
}

trait LowPriorityComponentRetrievers {
    given ambigFieldRetriever[ Outer, N <: Singleton, I ](
        using
        fr : ComponentRetriever.Aux[ Outer, FieldSelector[ N ], I ],
    ) : ComponentRetriever[ Outer, AmbigSelector[ N ] ] with {
        type Inner = I

        override def retrieve( from : Outer ) : I = fr.retrieve( from )
    }

    given ambigFieldTypeRetriever[ Outer, T, N <: Nat, I ](
        using
        fr : ComponentRetriever.Aux[ Outer, FieldSelector[ TypeSelector[ T, N ] ], I ],
    ) : ComponentRetriever[ Outer, AmbigSelector[ TypeSelector[ T, N ] ] ] with {
        type Inner = I

        override def retrieve( from : Outer ) : I = fr.retrieve( from )
    }

    given ambigSubTypeRetriever[ Outer, N <: Singleton, I ](
        using
        fr : ComponentRetriever.Aux[ Outer, SubTypeSelector[ N ], I ],
    ) : ComponentRetriever[ Outer, AmbigSelector[ N ] ] with {
        type Inner = I

        override def retrieve( from : Outer ) : I = fr.retrieve( from )
    }

    given ambigSubTypeTypeRetriever[ Outer, T, N <: Nat, I ](
        using
        fr : ComponentRetriever.Aux[ Outer, SubTypeSelector[ TypeSelector[ T, N ] ], I ],
    ) : ComponentRetriever[ Outer, AmbigSelector[ TypeSelector[ T, N ] ] ] with {
        type Inner = I

        override def retrieve( from : Outer ) : I = fr.retrieve( from )
    }
}

object ComponentRetriever extends LowPriorityComponentRetrievers {
    type Aux[ Outer, Sel, I ] = ComponentRetriever[ Outer, Sel ] { type Inner = I }

    given [ Outer ] : ComponentRetriever[ Outer, EmptyTuple ] with {
        type Inner = Outer

        override def retrieve( from : Outer ) : Outer = from
    }

    given [ Outer, SelHead, HeadInner, SelTail <: Tuple, I ](
        using
        hRetriever : ComponentRetriever.Aux[ Outer, SelHead, HeadInner ],
        tRetriever : ComponentRetriever.Aux[ HeadInner, SelTail, I ]
    ) : ComponentRetriever[ Outer, SelHead *: SelTail ] with {
        type Inner = I

        override def retrieve( from : Outer ) : I =
            tRetriever.retrieve( hRetriever.retrieve( from ) )
    }

    given fromFieldDescription[ T, F, N <: FieldName, S, SelR, I ](
        using
        ev : NotGiven[ SelR <:< Tuple ],
        fr : selector.ComponentRetriever.Aux[ Schema.Aux[ F, S ], SelR, I ],
    ) : ComponentRetriever[ Field[ T, F, N, S ], SelR ] with {
        override type Inner = I

        override def retrieve( from : Field[ T, F, N, S ] ) : Inner = {
            fr.retrieve( from.schema )
        }
    }

    given fromLazyFieldDescription[ T, F, N <: FieldName, SelR, I ](
        using
        ev : NotGiven[ SelR <:< Tuple ],
        sch : Schema[ F ],
        fr : selector.ComponentRetriever.Aux[ Schema.Aux[ F, sch.Shape ], SelR, I ],
    ) : ComponentRetriever[ LazyField[ T, F, N ], SelR ] with {
        override type Inner = I

        override def retrieve( from : LazyField[ T, F, N ] ) : Inner = {
            fr.retrieve( sch )
        }
    }

    given fromProductSchema[ SelN <: Singleton, T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Fld ](
        using
        fr : FieldRetriever.Aux[ SelN, R, Fld ],
    ) : ComponentRetriever[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ], FieldSelector[ SelN ] ] with {
        override type Inner = Fld

        override def retrieve( from : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ] ) : Fld = {
            fr.retrieve( from.shape.fieldDescriptions )
        }
    }

    given fromProductSchemaUsingType[ F, SelN <: Nat, T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Fld ](
        using
        fr : FieldTypeRetriever.Aux[ F, SelN, R, Fld ],
    ) : ComponentRetriever[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ], FieldSelector[ TypeSelector[ F, SelN ] ] ] with {
        override type Inner = Fld

        override def retrieve( from : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ] ) : Fld = {
            fr.retrieve( from.shape.fieldDescriptions )
        }
    }

    given fromProductSchemaBuilder[ SelN <: Singleton, T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, N <: FieldName, F, S ](
        using
        fr : FieldRetriever.Aux[ SelN, R, Field[ T, F, N, S ] ],
    ) : ComponentRetriever[ ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ], FieldSelector[ SelN ] ] with {
        override type Inner = Field[ T, F, N, S ]

        override def retrieve( from : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] ) : Field[ T, F, N, S ] = {
            fr.retrieve( from.fieldDescs )
        }
    }

    given fromProductSchemaBuilderUsingType[ F, SelN <: Nat, T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, N <: FieldName, Fld ](
        using
        fr : FieldTypeRetriever.Aux[ F, SelN, R, Fld ],
    ) : ComponentRetriever[ ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ], FieldSelector[ TypeSelector[ F, SelN ] ] ] with {
        override type Inner = Fld

        override def retrieve( from : ProductSchemaBuilder[ T, R, RV, AF, AFS, AFE, C ] ) : Fld = {
            fr.retrieve( from.fieldDescs )
        }
    }

    given fromSubtypeDescription[ T, ST, D, DN, DV, N <: TypeName, STS, SelR, I ](
        using
        ev : NotGiven[ SelR <:< Tuple ],
        fr : selector.ComponentRetriever.Aux[ Schema.Aux[ ST, STS ], SelR, I ],
    ) : ComponentRetriever[ Subtype[ T, ST, D, DN, DV, N, STS ], SelR ] with {
        override type Inner = I

        override def retrieve( from : Subtype[ T, ST, D, DN, DV, N, STS ] ) : Inner = {
            fr.retrieve( from.schema )
        }
    }

    given fromLazySubtypeDescription[ T, ST, D, DN, DV, N <: TypeName, STS, SelR, I ](
        using
        ev : NotGiven[ SelR <:< Tuple ],
        sch : Schema.Aux[ ST, STS ],
        fr : selector.ComponentRetriever.Aux[ Schema.Aux[ ST, STS ], SelR, I ],
    ) : ComponentRetriever[ LazySubtype[ T, ST, D, DN, DV, N ], SelR ] with {
        override type Inner = I

        override def retrieve( from : LazySubtype[ T, ST, D, DN, DV, N ] ) : Inner = {
            fr.retrieve( sch )
        }
    }

    given fromCoproductSchema[ SelN <: Singleton, T, R <: Tuple, RV <: Tuple, D, DN, SubT <: Subtype.Subtype ](
        using
        fr : SubtypeRetriever.Aux[ SelN, R, SubT ],
    ) : ComponentRetriever[ Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ], SubTypeSelector[ SelN ] ] with {
        override type Inner = SubT

        override def retrieve( from : Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ] ) : SubT = {
            fr.retrieve( from.shape.subtypeDescriptions )
        }
    }

    given fromCoproductSchemaUsingType[ ST, SelN <: Nat, T, R <: Tuple, RV <: Tuple, D, DN, SubT ](
        using
        fr : SubtypeTypeRetriever.Aux[ ST, SelN, R, SubT ],
    ) : ComponentRetriever[ Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ], SubTypeSelector[ TypeSelector[ ST, SelN ] ] ] with {
        override type Inner = SubT

        override def retrieve( from : Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ] ) : SubT = {
            fr.retrieve( from.shape.subtypeDescriptions )
        }
    }

    given fromCoproductSchemaBuilder[ SelN <: Singleton, T, R <: Tuple, D, DN, ST ](
        using
        fr : SubtypeRetriever.Aux[ SelN, R, ST ],
    ) : ComponentRetriever[ CoproductSchemaBuilder[ T, R, D, DN ], SubTypeSelector[ SelN ] ] with {
        override type Inner = ST

        override def retrieve( from : CoproductSchemaBuilder[ T, R, D, DN ] ) : ST = {
            fr.retrieve( from.sts )
        }
    }

    given fromCoproductSchemaBuilderUsingType[ ST, SelN <: Nat, T, R <: Tuple, D, DN, SubT ](
        using
        fr : SubtypeTypeRetriever.Aux[ ST, SelN, R, SubT ],
    ) : ComponentRetriever[ CoproductSchemaBuilder[ T, R, D, DN ], SubTypeSelector[ TypeSelector[ ST, SelN ] ] ] with {
        override type Inner = SubT

        override def retrieve( from : CoproductSchemaBuilder[ T, R, D, DN ] ) : SubT = {
            fr.retrieve( from.sts )
        }
    }

    def retrieve[ Outer, Sel <: Tuple ](
        from : Outer,
    )(
        select : Selector[ Sel ],
    )(
        using
        cr : ComponentRetriever[ Outer, Sel ],
    ) : cr.Inner = cr.retrieve( from )
}
