package org.hungerford.generic.schema.types

trait Provider[ T ] {
    def provide : T
}

trait LowPriorityProviders {
    implicit def provideDerivation[ From, T ](
        implicit deriver: Deriver.Aux[ From, T ],
    ) : Provider[ T ] = new Provider[ T ] {
        override def provide : T = deriver.derive
    }
}

object Provider extends LowPriorityProviders {
    def apply[ T ](
        implicit pr : Provider[ T ],
    ) : Provider[ T ] = pr

    implicit def provideInstance[ T ](
        implicit inst : T,
    ) : Provider[ T ] = new Provider[T] {
        override def provide : T = inst
    }
}
