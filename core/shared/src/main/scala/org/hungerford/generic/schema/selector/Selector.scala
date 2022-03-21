package org.hungerford.generic.schema.selector

import org.hungerford.generic.schema.product.field.FieldName
import org.hungerford.generic.schema.types.{IntEquiv, Nat}

type Append[ R <: Tuple, T ] = Tuple.Concat[ R, T *: EmptyTuple ]

case class TypeSelector[ T, N <: Nat ]()

class Selector[ R <: Tuple ] {
    def /[ N <: Singleton ]( identifier : N ) : Selector[ Append[ R, AmbigSelector[ N ] ] ] = {
        new Selector[ Append[ R, AmbigSelector[ N ] ] ]
    }

    def /[ T, N <: Nat ]( ts : TypeSelector[ T, N ] ) : Selector[ Append[ R, AmbigSelector[ TypeSelector[ T, N ] ] ] ] = {
        new Selector[ Append[ R, AmbigSelector[ TypeSelector[ T, N ] ] ] ]
    }

    def /-[ N <: Singleton ]( field : N ) : Selector[ Append[ R, FieldSelector[ N ] ] ] = {
        new Selector[ Append[ R, FieldSelector[ N ] ] ]
    }

    def /-[ T, N <: Nat ]( ts : TypeSelector[ T, N ] ) : Selector[ Append[ R, FieldSelector[ TypeSelector[ T, N ] ] ] ] = {
        new Selector[ Append[ R, FieldSelector[ TypeSelector[ T, N ] ] ] ]
    }

    def /~[ N <: Singleton ]( subtype : N ) : Selector[ Append[ R, SubTypeSelector[ N ] ] ] = {
        new Selector[ Append[ R, SubTypeSelector[ N ] ] ]
    }

    def /~[ T, N <: Nat ]( ts : TypeSelector[ T, N ] ) : Selector[ Append[ R, SubTypeSelector[ TypeSelector[ T, N ] ] ] ] = {
        new Selector[ Append[ R, SubTypeSelector[ TypeSelector[ T, N ] ] ] ]
    }
}

trait FieldSelector[ N ]
trait SubTypeSelector[ N ]
trait AmbigSelector[  N ]

trait SelectorConversion {
    given [ N <: Singleton ] : Conversion[ N, Selector[ AmbigSelector[ N ] *: EmptyTuple ] ] with
        def apply( selector : N ) : Selector[ AmbigSelector[ N ] *: EmptyTuple ] =
            new Selector[ AmbigSelector[ N ] *: EmptyTuple ]

    given [ T, N <: Nat ] : Conversion[ TypeSelector[ T, N ], Selector[ AmbigSelector[ TypeSelector[ T, N ] ] *: EmptyTuple ] ] with
        def apply( selector : TypeSelector[ T, N ] ) : Selector[ AmbigSelector[ TypeSelector[ T, N ] ] *: EmptyTuple ] =
            new Selector[ AmbigSelector[ TypeSelector[ T, N ] ] *: EmptyTuple ]
}

trait SelectorDsl extends SelectorConversion {
    extension [ T1, N1 <: Nat ]( selection : TypeSelector[ T1, N1 ] )
        def /[ T2, N2 <: Nat ]( field : TypeSelector[ T2, N2 ] ) :
            Selector[ AmbigSelector[ TypeSelector[ T1, N1 ] ] *: AmbigSelector[ TypeSelector[ T2, N2 ] ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[TypeSelector[ T1, N1 ]] *: AmbigSelector[TypeSelector[ T2, N2 ]] *: EmptyTuple ]
            }

        def /[ N2 <: Singleton ]( field : N2 ) :
            Selector[ AmbigSelector[ TypeSelector[ T1, N1 ] ] *: AmbigSelector[ N2 ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[TypeSelector[ T1, N1 ]] *: AmbigSelector[N2] *: EmptyTuple ]
            }

        def /-[ T2, N2 <: Nat ]( sel : TypeSelector[ T2, N2 ] ) :
            Selector[ AmbigSelector[ TypeSelector[ T1, N1 ] ] *: FieldSelector[ TypeSelector[ T2, N2 ] ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[TypeSelector[ T1, N1 ]] *: FieldSelector[TypeSelector[ T2, N2 ]] *: EmptyTuple ]
            }

        def /-[ N2 <: Singleton ]( field : N2 ) :
            Selector[ AmbigSelector[ TypeSelector[ T1, N1 ] ] *: FieldSelector[ N2 ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[TypeSelector[ T1, N1 ]] *: FieldSelector[N2] *: EmptyTuple ]
            }

        def /~[ T2, N2 <: Nat ]( subtype : TypeSelector[ T2, N2 ] ) :
            Selector[ AmbigSelector[ TypeSelector[ T1, N1 ] ] *: SubTypeSelector[ TypeSelector[ T2, N2 ] ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[TypeSelector[ T1, N1 ]] *: SubTypeSelector[TypeSelector[ T2, N2 ]] *: EmptyTuple ]
            }

        def /~[ N2 <: Singleton ]( subtype : N2 ) :
            Selector[ AmbigSelector[ TypeSelector[ T1, N1 ] ] *: SubTypeSelector[ N2 ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[TypeSelector[ T1, N1 ]] *: SubTypeSelector[N2] *: EmptyTuple ]
            }

    extension [ N1 <: Singleton ]( selection : N1 )
        def /[ T2, N2 <: Nat ]( field : TypeSelector[ T2, N2 ] ) :
            Selector[ AmbigSelector[ N1 ] *: AmbigSelector[ TypeSelector[ T2, N2 ] ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[N1] *: AmbigSelector[TypeSelector[ T2, N2 ]] *: EmptyTuple ]
            }

        def /[ N2 <: Singleton ]( field : N2 ) :
            Selector[ AmbigSelector[ N1 ] *: AmbigSelector[ N2 ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[N1] *: AmbigSelector[N2] *: EmptyTuple ]
            }

        def /-[ T2, N2 <: Nat ]( sel : TypeSelector[ T2, N2 ] ) :
            Selector[ AmbigSelector[ N1 ] *: FieldSelector[ TypeSelector[ T2, N2 ] ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[N1] *: FieldSelector[TypeSelector[ T2, N2 ]] *: EmptyTuple ]
            }

        def /-[ N2 <: Singleton ]( field : N2 ) :
            Selector[ AmbigSelector[ N1 ] *: FieldSelector[ N2 ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[N1] *: FieldSelector[N2] *: EmptyTuple ]
            }

        def /~[ T2, N2 <: Nat ]( subtype : TypeSelector[ T2, N2 ] ) :
            Selector[ AmbigSelector[ N1 ] *: SubTypeSelector[ TypeSelector[ T2, N2 ] ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[N1] *: SubTypeSelector[TypeSelector[ T2, N2 ]] *: EmptyTuple ]
            }

        def /~[ N2 <: Singleton ]( subtype : N2 ) :
            Selector[ AmbigSelector[ N1 ] *: SubTypeSelector[ N2 ] *: EmptyTuple ] = {
                new Selector[ AmbigSelector[N1] *: SubTypeSelector[N2] *: EmptyTuple ]
            }


    def select[ N <: Singleton ]( identifier: N ): Selector[ AmbigSelector[ N ] *: EmptyTuple ] = {
        new Selector[ AmbigSelector[ N ] *: EmptyTuple ]
    }

    def select[ T, N <: Nat ]( identifier: TypeSelector[ T, N ] ): Selector[ AmbigSelector[ TypeSelector[ T, N ] ] *: EmptyTuple ] = {
        new Selector[ AmbigSelector[ TypeSelector[ T, N ] ] *: EmptyTuple ]
    }

    def field[ N <: Singleton ]( field: N ): Selector[ FieldSelector[ N ] *: EmptyTuple ] = {
        new Selector[ FieldSelector[ N ] *: EmptyTuple ]
    }

    def field[ T, N <: Nat ]( field: TypeSelector[ T, N ] ): Selector[ FieldSelector[ TypeSelector[ T, N ] ] *: EmptyTuple ] = {
        new Selector[ FieldSelector[ TypeSelector[ T, N ] ] *: EmptyTuple ]
    }

    def subtype[ N <: Singleton ]( subtype : N ) : Selector[ SubTypeSelector[ N ] *: EmptyTuple ] = {
        new Selector[ SubTypeSelector[ N ] *: EmptyTuple ]
    }

    def subtype[ T, N <: Nat ]( subtype : TypeSelector[ T, N ] ) : Selector[ SubTypeSelector[ TypeSelector[ T, N ] ] *: EmptyTuple ] = {
        new Selector[ SubTypeSelector[ TypeSelector[ T, N ] ] *: EmptyTuple ]
    }

    def t[ T ] : TypeSelector[ T, Nat._0 ] = TypeSelector[ T, Nat._0 ]()
    def tN[ T ] : TS[ T ] = new TS[ T ]

    class TS[ T ] {
        def apply[ I <: Int & Singleton, N <: Nat ](
            skip : I,
        )(
            using
            conv : IntEquiv.Aux[ I, N ],
        ) : TypeSelector[ T, N ] = TypeSelector[ T, N ]()
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
