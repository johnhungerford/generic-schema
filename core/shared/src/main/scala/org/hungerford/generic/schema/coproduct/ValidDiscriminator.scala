package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, FieldGetter, FieldRetriever}
import org.hungerford.generic.schema.types.CtxWrapTuplesConstraint

trait ValidDiscriminator[ D, DN, R ]

object ValidDiscriminator {

    given [ R ] : ValidDiscriminator[ Unit, Nothing, R ] with {}

    given [ D, DN ] : ValidDiscriminator[ D, DN, EmptyTuple ] with {}

    given [ D, DN <: TypeName, DV, T, ST, N <: TypeName, STR <: Tuple, STRV <: Tuple, STAF, STAFS, STC, STDC, Tail <: Tuple, FT ](
        using
        next : ValidDiscriminator[ D, DN, Tail ],
    ) : ValidDiscriminator[ D, DN, Subtype.Aux[ T, ST, D, DN, DV, N, ProductShape[ ST, STR, STRV, STAF, STAFS, STC, STDC ] ] *: Tail ] with {}

}

