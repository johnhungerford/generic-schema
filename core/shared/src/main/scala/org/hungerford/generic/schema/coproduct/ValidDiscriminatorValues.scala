package org.hungerford.generic.schema.coproduct

trait ValidDiscriminatorValues[ D, R ]

object ValidDiscriminatorValues {
    given [ D ] : ValidDiscriminatorValues[ D, EmptyTuple ] with {}

    given [ D, DV <: D, Tail <: Tuple ](
        using
        ValidDiscriminatorValues[ D, Tail ],
    ) : ValidDiscriminatorValues[ D, DV *: Tail ] with {}
}
