package org.hungerford.generic.schema.types

trait ExistsFor[ TC[ _ ], T ]

trait LowPriorityExistsFors {
    given existsForType[ TC[ _ ], T ](
        using
        ev : TC[ T ],
    ) : ExistsFor[ TC, T ] with {}
}

object ExistsFor extends LowPriorityExistsFors {
    given existsForEmptyTuple[ TC[ _ ]  ] : ExistsFor[ TC, EmptyTuple ] with {}

    given existsForNonEmptyTuple[ TC[ _ ], H, Tail <: Tuple ](
        using
        h : ExistsFor[ TC, H ],
        t : ExistsFor[ TC, Tail ],
    ) : ExistsFor[ TC, H *: Tail ] with {}
}
