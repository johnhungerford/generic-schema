package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.product.field.FieldName

type Append[ R <: Tuple, T ] = Tuple.Concat[ R, T *: EmptyTuple ]

class Selector[ R <: Tuple ] {
    def /[ N <: Singleton ]( identifier : N ) : Selector[ Append[ R, AmbigSelector[ N ] ] ] = {
        new Selector[ Append[ R, AmbigSelector[ N ] ] ]
    }

    def /-[ N <: Singleton ]( field : N ) : Selector[ Append[ R, FieldSelector[ N ] ] ] = {
        new Selector[ Append[ R, FieldSelector[ N ] ] ]
    }

    def /~[ N <: Singleton ]( subtype : N ) : Selector[ Append[ R, SubTypeSelector[ N ] ] ] = {
        new Selector[ Append[ R, SubTypeSelector[ N ] ] ]
    }
}

trait FieldSelector[ N <: Singleton ]
trait SubTypeSelector[ N <: Singleton ]
trait AmbigSelector[  N <: Singleton ]

trait SelectorConversion {
    given [ N <: Singleton ] : Conversion[ N, Selector[ AmbigSelector[ N ] *: EmptyTuple ] ] with
        def apply( selector : N ) : Selector[ AmbigSelector[ N ] *: EmptyTuple ] =
            new Selector[ AmbigSelector[ N ] *: EmptyTuple ]
}

trait SelectorDsl extends SelectorConversion {
    extension [ N1 <: Singleton ]( selection : N1 ) def /[ N2 <: Singleton ]( field : N2 ) :
        Selector[ AmbigSelector[ N1 ] *: AmbigSelector[ N2 ] *: EmptyTuple ] = {
        new Selector[ AmbigSelector[N1] *: AmbigSelector[N2] *: EmptyTuple ]
    }

    extension [ N1 <: Singleton ]( selection : N1 ) def /-[ N2 <: Singleton ]( field : N2 ) :
        Selector[ AmbigSelector[ N1 ] *: FieldSelector[ N2 ] *: EmptyTuple ] = {
        new Selector[ AmbigSelector[N1] *: FieldSelector[N2] *: EmptyTuple ]
    }

    extension [ N1 <: Singleton ]( selection : N1 ) def /~[ N2 <: Singleton ]( subtype : N2 ) :
        Selector[ AmbigSelector[ N1 ] *: SubTypeSelector[ N2 ] *: EmptyTuple ] = {
        new Selector[ AmbigSelector[N1] *: SubTypeSelector[N2] *: EmptyTuple ]
    }

    def select[ N <: Singleton ]( identifier: N ): Selector[ AmbigSelector[ N ] *: EmptyTuple ] = {
        new Selector[ AmbigSelector[ N ] *: EmptyTuple ]
    }

    def field[ N <: Singleton ]( field: N ): Selector[ FieldSelector[ N ] *: EmptyTuple ] = {
        new Selector[ FieldSelector[ N ] *: EmptyTuple ]
    }

    def subtype[ N <: Singleton ]( subtype : N ) : Selector[ SubTypeSelector[ N ] *: EmptyTuple ] = {
        new Selector[ SubTypeSelector[ N ] *: EmptyTuple ]
    }
}

object Selector extends SelectorConversion {

    def apply[ N <: Singleton ]( identifier : N ) : Selector[ AmbigSelector[ N ] *: EmptyTuple ] = {
        new Selector[ AmbigSelector[ N ] *: EmptyTuple ]
    }

    def field[ N <: Singleton ]( field : N ) : Selector[ FieldSelector[ N ] *: EmptyTuple ] = {
        new Selector[ FieldSelector[ N ] *: EmptyTuple ]
    }

    def subtype[ N <: Singleton ]( subtype : N ) : Selector[ SubTypeSelector[ N ] *: EmptyTuple ] = {
        new Selector[ SubTypeSelector[ N ] *: EmptyTuple ]
    }

}
