package org.hungerford.generic.schema.types

trait Contains[ As <: Tuple, B ]

object Contains {
    given doesContain[ A, B, Tail <: Tuple ](
        using
        eq : => A =:= B,
    ) : Contains[ A *: Tail, B ] with {}

    given mightContain[ A, Tail <: Tuple, B ](
        using
        next : => Contains[ Tail, B ],
    ) : Contains[ A *: Tail, B ] with {}
}

trait Size[ Tp <: Tuple ] {
    def size : Int
}

object Size {
    given emptySize : Size[ EmptyTuple ] with {
        def size : Int = 0
    }

    given nonEmptySize[ Head, Tail <: Tuple ](
        using
        next : Size[ Tail ],
    ) : Size[ Head *: Tail ] with {
        def size : Int = next.size + 1
    }
}
