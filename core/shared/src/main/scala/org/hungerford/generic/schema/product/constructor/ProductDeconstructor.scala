package org.hungerford.generic.schema.product.constructor

import org.hungerford.generic.schema.product.field.Field
import scala.util.NotGiven

trait ProductDeconstructor[ T, Fs ] {
    type Res

    def deconstruct( value : T, informedBy : Fs ) : Res
}

object ProductDeconstructor {
    type Aux[ T, Fs, R ] = ProductDeconstructor[ T, Fs ] { type Res = R }

    given extractor[ T, F ] : ProductDeconstructor[ T, T => Map[ String, F ] ] with {
        type Res = Map[ String, F ]

        override def deconstruct( value: T, informedBy: T => Map[ String, F ] ): Map[ String, F ] = informedBy( value )
    }

    given field[ T, F, FD <: Field[ T, F ] ] : ProductDeconstructor[ T, FD ] with {
        type Res = F

        override def deconstruct( value : T, informedBy : FD ) : F =
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

    given noAfDeconstructor[ T, H, TailHead <: Tuple, TailHeadRes <: Tuple ](
        using
        hEv : NotGiven[ ProductDeconstructor[ T, H ] ],
        tdc : ProductDeconstructor.Aux[ T, TailHead, TailHeadRes ]
    ) : ProductDeconstructor[ T, H *: TailHead *: EmptyTuple ] with {
        type Res = TailHeadRes

        override def deconstruct(
            value: T, informedBy: H *: TailHead *: EmptyTuple
        ): TailHeadRes = tdc.deconstruct( value, informedBy.tail.head )
    }
}
