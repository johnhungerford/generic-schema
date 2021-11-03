package org.hungerford.generic.schema.types

trait Translator[ T ] {
    type Out

    def translate( t : T ) : Out
}

object Translator {
    type Aux[ T, Out0 ] = Translator[ T ] { type Out = Out0 }

    def apply[ T, Out ]( implicit tr : Translator.Aux[ T, Out ] ) : Translator.Aux[ T, Out ] = tr
}
