package org.hungerford.generic.schema.product.field

import scala.compiletime.{constValue, erasedValue, summonInline}

sealed trait Ineq[A, B]

object Ineq {
    given neq[A, B] : Ineq[ A, B ] with {}
    given neqAmbig2[A] : Ineq[ A, A ] with { ??? }
    given neqAmbig1[A] : Ineq[ A, A ] with { ??? }
}


type Stringleton = String & Singleton

sealed trait FieldDescriptionsDoNotContainFieldName[ N <: String, R <: Tuple ]

object FieldDescriptionsDoNotContainFieldName {
    given [ N <: Stringleton ] : FieldDescriptionsDoNotContainFieldName[ N, EmptyTuple ] with {}

    given [ N <: Stringleton, T, O <: Stringleton, S, Next <: Tuple ](
        using
        ev : => Ineq[ N, O ],
        nextEv : => FieldDescriptionsDoNotContainFieldName[ N, Next ],
    ) : FieldDescriptionsDoNotContainFieldName[ N, FieldDescription.Aux[ T, O, S ] *: Next ] with {}
}

sealed trait UniqueFieldNames[ R <: Tuple ]

object UniqueFieldNames {
    given UniqueFieldNames[ EmptyTuple ] with {}

    given [ T, N <: Stringleton, S, Rest <: Tuple ](
        using
        ev1 : FieldDescriptionsDoNotContainFieldName[ N, Rest ],
        ev2 : UniqueFieldNames[ Rest ],
    ) : UniqueFieldNames[ FieldDescription.Aux[ T, N, S ] *: Rest ] with {}
}
