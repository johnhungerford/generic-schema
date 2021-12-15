package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, FieldGetter, FieldRetriever}
import org.hungerford.generic.schema.types.CtxWrapTuplesConstraint

trait ValidDiscriminator[ D, DN, R ]

object ValidDiscriminator {

    given [ R ] : ValidDiscriminator[ Nothing, Nothing, R ] with {}

    given [ D, DN ] : ValidDiscriminator[ D, DN, EmptyTuple ] with {}

    given [ D, DN <: TypeName, T, N <: TypeName, STR <: Tuple, STRV <: Tuple, STAF, STAFS, STC, STDC, Tail <: Tuple, FT ](
        using
        fg : FieldRetriever.Aux[ DN, STR, FT ],
        ev : FT <:< Field[ D ],
        next : ValidDiscriminator[ D, DN, Tail ],
    ) : ValidDiscriminator[ D, DN, Subtype.Aux[ T, N, ProductShape[ T, STR, STRV, STAF, STAFS, STC, STDC ] ] *: Tail ] with {}

}

