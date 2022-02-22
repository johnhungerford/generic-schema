package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.{Schema, SchemaExtractor}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}

import scala.util.NotGiven

trait CoproductSchemaExtractor[ T, From ] {
    type Shape

    def extract( from : From ) : Schema.Aux[ T, Shape ]
}

trait CoproductSchemaExtractorPriority1 {
    given nextSubtypesExtractor[ ST, Head, Tail <: Tuple, S ](
        using
        next : CoproductSchemaExtractor.Aux[ ST, Tail, S ],
    ) : CoproductSchemaExtractor[ ST, Head *: Tail ] with {
        type Shape = S

        override def extract(
            from: Head *: Tail
        ): Schema.Aux[ ST, S ] = next.extract( from.tail )
    }
}

object CoproductSchemaExtractor extends CoproductSchemaExtractorPriority1 {
    type Aux[ T, From, S ] = CoproductSchemaExtractor[ T, From ] { type Shape = S }

    given subtypesCoproductExtractor[ ST, T, R <: Tuple, RV <: Tuple, D, DN, S ](
        using
        stExtr : CoproductSchemaExtractor.Aux[ ST, R, S ],
    ) : CoproductSchemaExtractor[ ST, CoproductShape[ T, R, RV, D, DN ] ] with {
        type Shape = S

        override def extract(
            from: CoproductShape[ T, R, RV, D, DN ]
        ): Schema.Aux[ ST, S ] = stExtr.extract( from.subtypeDescriptions )
    }

    given nestedCoproductExtractor[ ST, T, R <: Tuple, RV <: Tuple, D, DN, S ](
        using
        ev : NotGiven[ CoproductSchemaExtractor[ ST, R ] ],
        stExtr : NestedCoproductSchemaExtractor.Aux[ ST, R, S ],
    ) : CoproductSchemaExtractor[ ST, CoproductShape[ T, R, RV, D, DN ] ] with {
        type Shape = S

        override def extract(
            from: CoproductShape[ T, R, RV, D, DN ]
        ): Schema.Aux[ ST, S ] = stExtr.extract( from.subtypeDescriptions )
    }

    given subtypesHeadExtractor[ ST, T, D, DN, DV, N <: TypeName, S, SubT <: Subtype.Aux[ T, ST, D, DN, DV, N, S ], Tail <: Tuple ] : CoproductSchemaExtractor[ ST, SubT *: Tail ] with {
        type Shape = S

        override def extract(
            from: SubT *: Tail,
        ): Schema.Aux[ ST, S ] = from.head.schema
    }

    class Extr[ T ] {
        def apply[ Obj ](
            from : Obj,
        )(
            using
            extr : CoproductSchemaExtractor[ T, Obj ],
        ) : Schema.Aux[ T, extr.Shape ] = extr.extract( from )
    }

    def extract[ T ] : Extr[ T ] = new Extr[ T ]
}

trait NestedCoproductSchemaExtractor[ T, From ] {
    type Shape

    def extract( from : From ) : Schema.Aux[ T, Shape ]
}

trait NestedCoproductSchemaExtractorPriority1 {
    given nextHeadSubtypeExtractor[ ST, Head, Tail <: Tuple, S ](
        using
        next : NestedCoproductSchemaExtractor.Aux[ ST, Tail, S ],
    ) : NestedCoproductSchemaExtractor[ ST, Head *: Tail ] with {
        type Shape = S

        override def extract(
            from: Head *: Tail
        ): Schema.Aux[ ST, S ] = next.extract( from.tail )
    }
}

object NestedCoproductSchemaExtractor extends NestedCoproductSchemaExtractorPriority1 {
    type Aux[ T, From, S ] = NestedCoproductSchemaExtractor[ T, From ] { type Shape = S }

    given nestedHeadSubtypeExtractor[ ST, Head, Tail <: Tuple, S ](
        using
        schExtr : SchemaExtractor.Aux[ ST, Head, S ],
    ) : NestedCoproductSchemaExtractor[ ST, Head *: Tail ] with {
        type Shape = S

        override def extract(
            from: Head *: Tail
        ): Schema.Aux[ ST, S ] = schExtr.extract( from.head )
    }
}
