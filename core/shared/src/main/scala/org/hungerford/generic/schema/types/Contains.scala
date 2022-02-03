package org.hungerford.generic.schema.types

trait Contains[ As <: Tuple, B ]

object Contains {
    given doesContain[ A, Tail <: Tuple ] : Contains[ A *: Tail, A ] with {}

    given mightContain[ A, Tail <: Tuple, B ](
        using
        next : Contains[ Tail, A ],
    ) : Contains[ A *: Tail, B ] with {}
}
