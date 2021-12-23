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
    given ambigFieldRetriever[ Outer, N <: FieldName, I ](
        using
        fr : ComponentRetriever.Aux[ Outer, FieldSelector[ N ], I ],
    ) : ComponentRetriever[ Outer, AmbigSelector[ N ] ] with {
        type Inner = I

        override def retrieve( from : Outer ) : I = fr.retrieve( from )
    }

    given ambigSubTypeRetriever[ Outer, N <: FieldName, I ](
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

    given fromFieldDescription[ T, N <: FieldName, S, SelR, I ](
        using
        ev : NotGiven[ SelR <:< Tuple ],
        fr : selector.ComponentRetriever.Aux[ Schema.Aux[ T, S ], SelR, I ],
    ) : ComponentRetriever[ Field.Aux[ T, N, S ], SelR ] with {
        override type Inner = I

        override def retrieve( from : Field.Aux[ T, N, S ] ) : Inner = {
            fr.retrieve( from.schema )
        }
    }

    given fromProductSchema[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC, SelN <: FieldName, F, S ](
        using
        fr : FieldRetriever.Aux[ SelN, R, Field.Aux[ F, SelN, S ] ],
    ) : ComponentRetriever[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ], FieldSelector[ SelN ] ] with {
        override type Inner = Field.Aux[ F, SelN, S ]

        override def retrieve( from : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ] ) : Field.Aux[ F, SelN, S ] = {
            fr.retrieve( from.shape.fieldDescriptions )
        }
    }

    given fromProductSchemaBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC, SelN <: FieldName, F, S ](
        using
        fr : FieldRetriever.Aux[ SelN, R, Field.Aux[ F, SelN, S ] ],
    ) : ComponentRetriever[ ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ], FieldSelector[ SelN ] ] with {
        override type Inner = Field.Aux[ F, SelN, S ]

        override def retrieve( from : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] ) : Field.Aux[ F, SelN, S ] = {
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

    given fromCoproductSchema[ T, R <: Tuple, RV <: Tuple, D, DN, SelN <: TypeName, ST, DV, STS ](
        using
        fr : SubtypeRetriever.Aux[ SelN, R, Subtype.Aux[ T, ST, D, DN, DV, SelN, STS] ],
    ) : ComponentRetriever[ Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ], SubTypeSelector[ SelN ] ] with {
        override type Inner = Subtype.Aux[ T, ST, D, DN, DV, SelN, STS]

        override def retrieve( from : Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ] ) : Inner = {
            fr.retrieve( from.shape.subtypeDescriptions )
        }
    }

    given fromCoproductSchemaBuilder[ T, R <: Tuple, D, DN, SelN <: TypeName,  ST ](
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
