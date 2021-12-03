package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldName, FieldReplacer, FieldRetriever}
import org.hungerford.generic.schema.product.{ProductSchemaBuilder, ProductShape}


trait ComponentRetriever[ Outer, Sel ] {
    type Inner

    def retrieve( from : Outer ) : Inner
}

object ComponentRetriever {
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

    given fromProductFieldDescription[ T, N <: FieldName, R <: Tuple, RV <: Tuple, AF, AFS, C, DC, SelN <: FieldName, F, S ](
        using
        fr : FieldRetriever.Aux[ SelN, R, FieldDescription.Aux[ F, SelN, S ] ],
    ) : ComponentRetriever[ FieldDescription.Aux[ T, N, ProductShape[ T, R, RV, AF, AFS, C, DC ] ], FieldSelector[ SelN ] ] with {
        override type Inner = FieldDescription.Aux[ F, SelN, S ]

        override def retrieve( from : FieldDescription.Aux[ T, N, ProductShape[ T, R, RV, AF, AFS, C, DC ] ] ) : FieldDescription.Aux[ F, SelN, S ] = {
            fr.retrieve( from.schema.shape.fieldDescriptions )
        }
    }

//    given fromProductFieldDescriptionAmbigSelector[ T, N <: FieldName, R <: Tuple, RV <: Tuple, AF, AFS, C, DC, SelN <: FieldName, F, S ](
//        using
//        fr : FieldRetriever.Aux[ SelN, R, FieldDescription.Aux[ F, SelN, S ] ],
//    ) : ComponentRetriever[ FieldDescription.Aux[ T, N, ProductShape[ T, R, RV, AF, AFS, C, DC ] ], AmbigSelector[ SelN ] ] with {
//        override type Inner = FieldDescription.Aux[ F, SelN, S ]
//
//        override def retrieve( from : FieldDescription.Aux[ T, N, ProductShape[ T, R, RV, AF, AFS, C, DC ] ] ) : FieldDescription.Aux[ F, SelN, S ] = {
//            fr.retrieve( from.schema.shape.fieldDescriptions )
//        }
//    }

    given fromProductSchema[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC, SelN <: FieldName, F, S ](
        using
        fr : FieldRetriever.Aux[ SelN, R, FieldDescription.Aux[ F, SelN, S ] ],
    ) : ComponentRetriever[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ], FieldSelector[ SelN ] ] with {
        override type Inner = FieldDescription.Aux[ F, SelN, S ]

        override def retrieve( from : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ] ) : FieldDescription.Aux[ F, SelN, S ] = {
            fr.retrieve( from.shape.fieldDescriptions )
        }
    }

//    given fromProductSchemaAmbigSelector[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC, SelN <: FieldName, F, S ](
//        using
//        fr : FieldRetriever.Aux[ SelN, R, FieldDescription.Aux[ F, SelN, S ] ],
//    ) : ComponentRetriever[ Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ], AmbigSelector[ SelN ] ] with {
//        override type Inner = FieldDescription.Aux[ F, SelN, S ]
//
//        override def retrieve( from : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ] ) : FieldDescription.Aux[ F, SelN, S ] = {
//            fr.retrieve( from.shape.fieldDescriptions )
//        }
//    }

    given fromProductSchemaBuilder[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC, SelN <: FieldName, F, S ](
        using
        fr : FieldRetriever.Aux[ SelN, R, FieldDescription.Aux[ F, SelN, S ] ],
    ) : ComponentRetriever[ ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ], FieldSelector[ SelN ] ] with {
        override type Inner = FieldDescription.Aux[ F, SelN, S ]

        override def retrieve( from : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] ) : FieldDescription.Aux[ F, SelN, S ] = {
            fr.retrieve( from.fieldDescs )
        }
    }

//    given fromProductSchemaBuilderAmbig[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC, SelN <: FieldName, F, S ](
//        using
//        fr : FieldRetriever.Aux[ SelN, R, FieldDescription.Aux[ F, SelN, S ] ],
//    ) : ComponentRetriever[ ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ], AmbigSelector[ SelN ] ] with {
//        override type Inner = FieldDescription.Aux[ F, SelN, S ]
//
//        override def retrieve( from : ProductSchemaBuilder[ T, R, RV, AF, AFS, C, DC ] ) : FieldDescription.Aux[ F, SelN, S ] = {
//            fr.retrieve( from.fieldDescs )
//        }
//    }

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

    def retrieve[ T, S, Sel <: Tuple ](
        from : Schema.Aux[ T, S ],
    )(
        select : Selector[ Sel ],
    )(
        using
        cr : ComponentRetriever[ Schema.Aux[ T, S ], Sel ],
    ) : cr.Inner = cr.retrieve( from )
}
