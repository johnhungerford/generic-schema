package org.hungerford.generic.schema.types

trait Deriver[ From ] {
    type Out

    def derive : Out
}

object Deriver {
    type Aux[ From, T ] = Deriver[ From ] { type Out = T }
    def apply[ From, T ](
        implicit der : Deriver.Aux[ From, T ],
    ) : Deriver.Aux[ From, T ] = der


}
