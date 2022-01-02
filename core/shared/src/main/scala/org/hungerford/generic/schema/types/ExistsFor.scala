package org.hungerford.generic.schema.types

trait ExistsFor[ TC[ _ ], T ]

object ExistsFor {
    given existsForEmptyTuple[ TC[ _ ]  ] : ExistsFor[ TC, EmptyTuple ] with {}

    given existsForNonEmptyTuple[ TC[ _ ], H, Tail <: Tuple, R <: H *: Tail ](
        using
        h : TC[ H ],
        t : ExistsFor[ TC, Tail ],
    ) : ExistsFor[ TC, R ] with {}
}
