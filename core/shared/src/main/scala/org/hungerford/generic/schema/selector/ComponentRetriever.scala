package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.{Schema, selector}
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, SubtypeRetriever, TypeName}
import org.hungerford.generic.schema.coproduct.{CoproductSchemaBuilder, CoproductShape, subtype}
import org.hungerford.generic.schema.product.field.{Field, FieldName, FieldReplacer, FieldRetriever}
import org.hungerford.generic.schema.product.{ProductSchemaBuilder, ProductShape}

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

    given ambigSubTypeRetriever[ Outer, N <: Singleton, I ](
        using
        fr : ComponentRetriever.Aux[ Outer, SubTypeSelector[ N ], I ],
    ) : ComponentRetriever[ Outer, AmbigSelector[ N ] ] with {
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

    given fromProductSchema[ SelN <: Singleton, T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, N <: FieldName, F, S ](
        using
        fr : FieldRetriever.Aux[ SelN, R, Field[ T, F, N, S ] ],
    ) : ComponentRetriever[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ], FieldSelector[ SelN ] ] with {
        override type Inner = Field[ T, F, N, S ]

        override def retrieve( from : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ] ) : Field[ T, F, N, S ] = {
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

    given fromSubtypeDescription[ T, ST, D, DN, DV, N <: TypeName, STS, SelR, I ](
        using
        ev : NotGiven[ SelR <:< Tuple ],
        fr : selector.ComponentRetriever.Aux[ Schema.Aux[ ST, STS ], SelR, I ],
    ) : ComponentRetriever[ Subtype.Aux[ T, ST, D, DN, DV, N, STS ], SelR ] with {
        override type Inner = I

        override def retrieve( from : Subtype.Aux[ T, ST, D, DN, DV, N, STS ] ) : Inner = {
            fr.retrieve( from.schema )
        }
    }

    given fromCoproductSchema[ SelN <: Singleton, T, R <: Tuple, RV <: Tuple, D, DN, N <: TypeName, ST, DV, STS ](
        using
        fr : SubtypeRetriever.Aux[ SelN, R, Subtype.Aux[ T, ST, D, DN, DV, N, STS] ],
    ) : ComponentRetriever[ Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ], SubTypeSelector[ SelN ] ] with {
        override type Inner = Subtype.Aux[ T, ST, D, DN, DV, N, STS]

        override def retrieve( from : Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ] ) : Inner = {
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

    def retrieve[ Outer, Sel <: Tuple ](
        from : Outer,
    )(
        select : Selector[ Sel ],
    )(
        using
        cr : ComponentRetriever[ Outer, Sel ],
    ) : cr.Inner = cr.retrieve( from )
}
