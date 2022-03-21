package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, TypeName}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, FieldGetter, FieldRetriever}
import org.hungerford.generic.schema.types.CtxWrapTuplesConstraint

import scala.util.NotGiven

trait ValidDiscriminator[ D, DN, R ]

object ValidDiscriminator {

    given [ R ] : ValidDiscriminator[ Unit, Unit, R ] with {}

    given [ D, DN ] : ValidDiscriminator[ D, DN, EmptyTuple ] with {}

    given [ D, DN <: TypeName, DV <: D & Singleton, T, ST, N <: TypeName, STR <: Tuple, STRV <: Tuple, STAF, STAFS, STAFE, STC, Tail <: Tuple, FT ](
        using
        next : ValidDiscriminator[ D, DN, Tail ],
    ) : ValidDiscriminator[ D, DN, Subtype[ T, ST, D, DN, DV, N, ProductShape[ ST, STR, STRV, STAF, STAFS, STAFE, STC ] ] *: Tail ] with {}

    given lazyVd[ D, DN <: TypeName, DV <: D & Singleton, T, ST, N <: TypeName, STR <: Tuple, STRV <: Tuple, STAF, STAFS, STAFE, STC, Tail <: Tuple, FT ](
        using
        sch: Schema.Aux[ ST, ProductShape[ ST, STR, STRV, STAF, STAFS, STAFE, STC ] ],
        next : ValidDiscriminator[ D, DN, Tail ],
    ) : ValidDiscriminator[ D, DN, LazySubtype[ T, ST, D, DN, DV, N ] *: Tail ] with {}

}

trait UniqueDiscriminatorValues[ R ]

trait LowPriorityUDVs {
    given [ T, ST, D, DN, DV, N <: TypeName, SubT <: Subtype.OrLazy[ T, ST, D, DN, DV, N ], Tail <: Tuple ](
        using
        dnc : DoesNotContainDV[ Tail, DV ],
        next : UniqueDiscriminatorValues[ Tail ],
    ) : UniqueDiscriminatorValues[ SubT *: Tail ] with {}
}

object UniqueDiscriminatorValues extends LowPriorityUDVs {
    given UniqueDiscriminatorValues[ EmptyTuple ] with {}

    given ignoreUnitDVs[ T, ST, D, DN, N <: TypeName,  SubT <: Subtype.OrLazy[ T, ST, D, DN, Unit, N ], Tail <: Tuple ](
        using
        next : UniqueDiscriminatorValues[ Tail ],
    ) : UniqueDiscriminatorValues[ SubT *: Tail ] with {}
}

trait UniqueTypeNames[ R ]

object UniqueTypeNames {
    given UniqueTypeNames[ EmptyTuple ] with {}

    given [ N <: TypeName, SubT <: Subtype.Named[ N ], Tail <: Tuple ](
        using
        dnc : DoesNotContainN[ Tail, N ],
        next : UniqueTypeNames[ Tail ],
    ) : UniqueTypeNames[ SubT *: Tail ] with {}
}

trait DoesNotContainDV[ R, DV ]

object DoesNotContainDV {
    given [ DV ] : DoesNotContainDV[ EmptyTuple, DV ] with {}

    given [ D, DN, DVDiff, N <: TypeName, SubT <: Subtype.Discr[ D, DN, DVDiff ], Tail <: Tuple, DV ](
        using
        ev : NotGiven[ DV =:= DVDiff ],
        next : DoesNotContainDV[ Tail, DV ],
    ) : DoesNotContainDV[ SubT *: Tail, DV ] with {}
}

trait DoesNotContainN[ R, N ]

object DoesNotContainN {
    given [ N ] : DoesNotContainN[ EmptyTuple, N ] with {}

    given [ NDiff <: TypeName, SubT <: Subtype.Named[ NDiff ], Tail <: Tuple, N ](
        using
        ev : NotGiven[ N =:= NDiff ],
        next : DoesNotContainN[ Tail, N ],
    ) : DoesNotContainN[ SubT *: Tail, N ] with {}
}
