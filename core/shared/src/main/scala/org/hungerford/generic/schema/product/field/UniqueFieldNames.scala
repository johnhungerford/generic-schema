package org.hungerford.generic.schema.product.field

import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.util.NotGiven

type Ineq[ A, B ] = NotGiven[ A =:= B ]

type Stringleton = String & Singleton

sealed trait FieldDescriptionsDoNotContainFieldName[ N <: String, R <: Tuple ]

object FieldDescriptionsDoNotContainFieldName {
    given [ N <: Stringleton ] : FieldDescriptionsDoNotContainFieldName[ N, EmptyTuple ] with {}

    given [ N <: Stringleton, T, O <: Stringleton, S, Next <: Tuple ](
        using
        ev : => Ineq[ N, O ],
        nextEv : => FieldDescriptionsDoNotContainFieldName[ N, Next ],
    ) : FieldDescriptionsDoNotContainFieldName[ N, Field.Aux[ T, O, S ] *: Next ] with {}
}

sealed trait UniqueFieldNames[ R <: Tuple ]

object UniqueFieldNames {
    given UniqueFieldNames[ EmptyTuple ] with {}

    given [ T, N <: Stringleton, S, Rest <: Tuple ](
        using
        ev1 : FieldDescriptionsDoNotContainFieldName[ N, Rest ],
        ev2 : UniqueFieldNames[ Rest ],
    ) : UniqueFieldNames[ Field.Aux[ T, N, S ] *: Rest ] with {}
}
