package org.hungerford.generic.schema.types

sealed trait Zipper[ A, B ] {
    type Out
}

object Zipper {
    type Aux[ A, B, Res ] = Zipper[ A, B ] {
        type Out = Res
    }

    inline given Zipper[ EmptyTuple, EmptyTuple ] with {
        type Out = EmptyTuple
    }

    inline given[ HeadL, TailL <: Tuple, HeadR, TailR <: Tuple, Res <: Tuple ] (
        using zip: Zipper.Aux[ TailL, TailR, Res ],
    ): Zipper[ HeadL *: TailL, HeadR *: TailR ] with {
        type Out = (HeadL, HeadR) *: zip.Out
    }
}
