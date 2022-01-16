package org.hungerford.generic.schema.product.constructor

import org.hungerford.generic.schema.product.field.Field

trait ProductDeconstructor[ T, Fs ] {
    type Res

    def deconstruct( value : T, informedBy : Fs ) : Res
}

object ProductDeconstructor[ T, R ] {
    type Aux[ T, Fs, R ] = ProductDeconstructor[ T, Fs ] { type Res = R }

    given extractor[ T, F ] : ProductDeconstructor[ T, T => F ] with {
        type Res = F

        override def deconstruct( value: T, informedBy: T => F ): F = informedBy( value )
    }

    given field[ T, F ] : ProductDeconstructor[ T, Field[ T, F ] ] with {
        type Res = F

        override def deconstruct( value : T, informedBy : Field[ T, F ] ) : F =
            informedBy.extractor( value )
    }

    given [ T ] : ProductDeconstructor[ T, EmptyTuple ] with {
        type Res = EmptyTuple

        override def deconstruct( value: T, informedBy : EmptyTuple ) : EmptyTuple = EmptyTuple
    }

    given [ T, H, Tail <: Tuple, HRes, TailRes <: Tuple ](
        using
        hdc : ProductDeconstructor.Aux[ T, H , HRes ],
        tdc : ProductDeconstructor.Aux[ T, Tail, TailRes ]
    ) : ProductDeconstructor[ T, H *: Tail ] with {
        type Res = HRes *: TailRes

        override def deconstruct( value: T, informedBy: H *: Tail ): HRes *: TailRes =
            hdc.deconstruct( value, informedBy.head ) *: tdc.deconstruct( value, informedBy.tail )
    }
}
