package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, FieldGetter, FieldRetriever}
import org.hungerford.generic.schema.types.CtxWrapTuplesConstraint

import scala.util.NotGiven

trait ValidDiscriminator[ D, DN, R ]

object ValidDiscriminator {

    given [ R ] : ValidDiscriminator[ Unit, Nothing, R ] with {}

    given [ D, DN ] : ValidDiscriminator[ D, DN, EmptyTuple ] with {}

    given [ D, DN <: TypeName, DV <: D & Singleton, T, ST, N <: TypeName, STR <: Tuple, STRV <: Tuple, STAF, STAFS, STAFE, STC, Tail <: Tuple, FT ](
        using
        next : ValidDiscriminator[ D, DN, Tail ],
    ) : ValidDiscriminator[ D, DN, Subtype.Aux[ T, ST, D, DN, DV, N, ProductShape[ ST, STR, STRV, STAF, STAFS, STAFE, STC ] ] *: Tail ] with {}

}

trait UniqueDiscriminatorValues[ R ]

trait LowPriorityUDVs {
    given [ T, ST, D, DN, DV, N <: TypeName, S, Tail <: Tuple ](
        using
        dnc : DoesNotContainDV[ Tail, DV ],
        next : UniqueDiscriminatorValues[ Tail ],
    ) : UniqueDiscriminatorValues[ Subtype.Aux[ T, ST, D, DN, DV, N, S ] *: Tail ] with {}
}

object UniqueDiscriminatorValues extends LowPriorityUDVs {
    given UniqueDiscriminatorValues[ EmptyTuple ] with {}

    given ignoreUnitDVs[ T, ST, D, DN, N <: TypeName, S, Tail <: Tuple ](
        using
        next : UniqueDiscriminatorValues[ Tail ],
    ) : UniqueDiscriminatorValues[ Subtype.Aux[ T, ST, D, DN, Unit, N, S ] *: Tail ] with {}
}

trait UniqueTypeNames[ R ]

object UniqueTypeNames {
    given UniqueTypeNames[ EmptyTuple ] with {}

    given [ T, ST, D, DN, DV, N <: TypeName, S, Tail <: Tuple ](
        using
        dnc : DoesNotContainN[ Tail, N ],
        next : UniqueTypeNames[ Tail ],
    ) : UniqueTypeNames[ Subtype.Aux[ T, ST, D, DN, DV, N, S ] *: Tail ] with {}
}

trait DoesNotContainDV[ R, DV ]

object DoesNotContainDV {
    given [ DV ] : DoesNotContainDV[ EmptyTuple, DV ] with {}

    given [ T, ST, D, DN, DVDiff, N <: TypeName, S, Tail <: Tuple, DV ](
        using
        ev : NotGiven[ DV =:= DVDiff ],
        next : DoesNotContainDV[ Tail, DV ],
    ) : DoesNotContainDV[ Subtype.Aux[ T, ST, D, DN, DVDiff, N, S ] *: Tail, DV ] with {}
}

trait DoesNotContainN[ R, N ]

object DoesNotContainN {
    given [ N ] : DoesNotContainN[ EmptyTuple, N ] with {}

    given [ T, ST, D, DN, DV, NDiff <: TypeName, S, Tail <: Tuple, N ](
        using
        ev : NotGiven[ N =:= NDiff ],
        next : DoesNotContainN[ Tail, N ],
    ) : DoesNotContainN[ Subtype.Aux[ T, ST, D, DN, DV, NDiff, S ] *: Tail, N ] with {}
}
