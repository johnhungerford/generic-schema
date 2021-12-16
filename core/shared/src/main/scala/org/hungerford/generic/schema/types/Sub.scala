package org.hungerford.generic.schema.types

sealed trait Sub[ A, B ] {
    type ASub = A & B

    def apply( from : A ) : ASub
}

object Sub {
    type Aux[ A, B, AS ] = Sub[ A, B ] { type ASub = AS }

    given [ A, B ]( using ev : A <:< B ) : Sub[ A, B ] with {
        type AX[ +X ] = A & X

        override def apply( from: A ): ASub = ev.liftCo[ AX ]( from )
    }
}
