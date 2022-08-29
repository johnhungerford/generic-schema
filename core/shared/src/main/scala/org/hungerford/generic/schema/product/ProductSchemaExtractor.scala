package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.{Schema, SchemaExtractor}
import org.hungerford.generic.schema.product.field.Field

import scala.util.NotGiven

trait ProductSchemaExtractor[ T, From ] {
    type Shape

    def extract( from : From ) : Schema.Aux[ T, Shape ]
}

trait ProductSchemaExtractorPriority3 {
    given afExtractor[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ](
        using
        ev : NotGiven[ ProductSchemaExtractor[ AF, R ] ],
    ) : ProductSchemaExtractor[ AF, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ] with {
        type Shape = AFS

        override def extract(
            from: ProductShape[ T, R, RV, AF, AFS, AFE, C ],
        ) : Schema.Aux[ AF, AFS ] = from.additionalFieldsSchema
    }
}

trait ProductSchemaExtractorPriority2 extends ProductSchemaExtractorPriority3 {
    given nextExtractor[ T, Head, Tail <: Tuple, Res ](
        using
        next : ProductSchemaExtractor.Aux[ T, Tail, Res ],
    ) : ProductSchemaExtractor[ T, Head *: Tail ] with {
        type Shape = Res

        override def extract(
            from: Head *: Tail
        ): Aux[ T, Res ] = next.extract( from.tail )
    }
}

trait ProductSchemaExtractorPriority1 extends ProductSchemaExtractorPriority2 {
    given headExtractor[ T, S, Fld <: Field.Shaped[ T, S ], Tail <: Tuple ] : ProductSchemaExtractor[ T, Fld *: Tail ] with {
        type Shape = S

        override def extract(
            from: Fld *: Tail
        ) : Aux[ T, S ] =
            val fld: Fld = from.head
            fld.schema
    }

    given nestedExtractor[ F, T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, S ](
        using
        ev : NotGiven[ ProductSchemaExtractor[ F, R ] ],
        nestExtr : NestedProductSchemaExtractor.Aux[ F, R, S ],
    ) : ProductSchemaExtractor[ F, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ] with {
        type Shape = S

        override def extract(
            from: ProductShape[ T, R, RV, AF, AFS, AFE, C ],
        ) : Schema.Aux[ F, S ] = nestExtr.extract( from.fieldDescriptions )
    }
}

object ProductSchemaExtractor extends ProductSchemaExtractorPriority1 {
    type Aux[ T, From, S ] = ProductSchemaExtractor[ T, From ] { type Shape = S }

    given fieldsExtractor[ F, T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, S ](
        using
        fieldsExtr : ProductSchemaExtractor.Aux[ F, R, S ],
    ) : ProductSchemaExtractor[ F, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ] with {
        type Shape = S

        override def extract(
            from: ProductShape[ T, R, RV, AF, AFS, AFE, C ]
        ) : Schema.Aux[ F, S ] = fieldsExtr.extract( from.fieldDescriptions )
    }

    class Extr[ T ] {
        def apply[ Obj ](
            obj: Obj
        )(
            using
            extr: ProductSchemaExtractor[ T, Obj ]
        ): Schema.Aux[ T, extr.Shape ] = extr.extract( obj )
    }

    def extract[ T ] = new Extr[ T ]
}

trait NestedProductSchemaExtractor[ T, From ] {
    type Shape

    def extract( from : From ) : Schema.Aux[ T, Shape ]
}

trait NestedProductSchemaExtractorPriority1 {
    given nestedNextExtractor[ T, Head, Tail <: Tuple, S ](
        using
        nextExtr : NestedProductSchemaExtractor.Aux[ T, Tail, S ],
    ) : NestedProductSchemaExtractor[ T, Head *: Tail ] with {
        override type Shape = S

        override def extract(
            from: Head *: Tail
        ): Aux[ T, S ] = nextExtr.extract( from.tail )
    }
}

object NestedProductSchemaExtractor extends NestedProductSchemaExtractorPriority1 {
    type Aux[ T, From, S ] = NestedProductSchemaExtractor[ T, From ] { type Shape = S }

    given nestedHeadExtractor[ T, Head, Tail <: Tuple, S ](
        using
        extr : SchemaExtractor.Aux[ T, Head, S ],
    ) : NestedProductSchemaExtractor[ T, Head *: Tail ] with {
        type Shape = S

        override def extract(
            from: Head *: Tail
        ) : Schema.Aux[ T, S ] = extr.extract( from.head )
    }
}
