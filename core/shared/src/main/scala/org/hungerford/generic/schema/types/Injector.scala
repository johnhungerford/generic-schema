package org.hungerford.generic.schema.types

trait Injector[ T, Target, Using ] {
    type Out

    def inject( value : T, into : Target, using : Using ) : Out
}

object Injector {
    type Aux[ T, Target, Using, Out0 ] = Injector[ T, Target, Using ] { type Out = Out0 }
}
