package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.product.field.FieldName

type Append[ R <: Tuple, T ] = Tuple.Concat[ R, T *: EmptyTuple ]

class Selector[ R <: Tuple ] {
    def /[ N <: FieldName ]( field : N ) : Selector[ Append[ R, FieldSelector[ N ] ] ] = {
        new Selector[ Append[ R, FieldSelector[ N ] ] ]
    }

    def /~[ N <: FieldName ]( subtype : N ) : Selector[ Append[ R, SubTypeSelector[ N ] ] ] = {
        new Selector[ Append[ R, SubTypeSelector[ N ] ] ]
    }
}

trait FieldSelector[ N <: FieldName ]
trait SubTypeSelector[ N <: FieldName ]
trait AmbigSelector[  N <: FieldName ]

object Selector {

    def /[ N <: FieldName ]( field : N ) : Selector[ FieldSelector[ N ] *: EmptyTuple ] = {
        new Selector[ FieldSelector[ N ] *: EmptyTuple ]
    }

    def /~[ N <: FieldName ]( subtype : N ) : Selector[ SubTypeSelector[ N ] *: EmptyTuple ] = {
        new Selector[ SubTypeSelector[ N ] *: EmptyTuple ]
    }

    extension [ N1 <: FieldName ]( selection : N1 ) def /[ N2 <: FieldName ]( field : N2 ) :
        Selector[ AmbigSelector[ N1 ] *: FieldSelector[ N2 ] *: EmptyTuple ] = {
        new Selector[ AmbigSelector[N1] *: FieldSelector[N2] *: EmptyTuple ]
    }

    extension [ N1 <: FieldName ]( selection : N1 ) def /~[ N2 <: FieldName ]( subtype : N2 ) :
        Selector[ AmbigSelector[ N1 ] *: SubTypeSelector[ N2 ] *: EmptyTuple ] = {
        new Selector[ AmbigSelector[N1] *: SubTypeSelector[N2] *: EmptyTuple ]
    }

}
