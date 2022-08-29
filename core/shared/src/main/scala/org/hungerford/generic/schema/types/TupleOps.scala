package org.hungerford.generic.schema.types

import scala.compiletime.ops.int.{-, <=, >}
import scala.deriving.Mirror
import scala.util.NotGiven

trait Remover[ I, R <: Tuple ] {
    type Out <: Tuple

    def remove( tuple : R ) : Out
}

trait LowPriorityRemovers {
    given continue[ I <: Nat, DecI <: Nat, Head, Tail <: Tuple, Res <: Tuple ](
        using
        ev : Nat.DecA[ I, DecI ],
        next : Remover.Aux[ DecI, Tail, Res ],
    ) : Remover[ I, Head *: Tail ] with {
        type Out = Head *: Res

        override def remove( tuple: Head *: Tail ): Out = tuple.head *: next.remove( tuple.tail )
    }

}

object Remover extends LowPriorityRemovers {
    type Aux[ I, R <: Tuple, O ] = Remover[ I, R ] { type Out = O }

    given removeHead[ I <: Nat, Head, Tail <: Tuple ] : Remover[ Nat._0, Head *: Tail ] with {
        type Out = Tail

        override def remove( tuple: Head *: Tail ): Out = tuple.tail
    }

    given equiv[ I <: Int, N <: Nat, R <: Tuple, Res <: Tuple ](
        using
        ev : Nat.IntA[ I, N ],
        rm : Remover.Aux[ N, R, Res ],
    ) : Remover[ I, R ] with {
        type Out = Res

        override def remove( tuple: R ): Out = rm.remove( tuple )
    }

    sealed class RM[ I ] {
        def apply[ R <: Tuple ](
            tuple : R,
        )(
            using
            rm : Remover[ I, R ],
        ) : rm.Out = rm.remove( tuple )
    }

    def remove[ I ] : RM[ I ] = new RM[ I ]
}

trait Replacer[ I, R <: Tuple, New ] {
    type Out <: Tuple

    def replace( elems : R, withElement : New ) : Out
}

trait LowPriorityReplacers {
    given continue[ I <: Nat, DecI <: Nat, Head, Tail <: Tuple, TailRes <: Tuple, New ](
        using
        ev : Nat.DecA[ I, DecI ],
        replacer : Replacer.Aux[ DecI, Tail, New, TailRes ],
    ) : Replacer[ I, Head *: Tail, New ] with {
        type Out = Head *: TailRes

        override def replace( elems: Head *: Tail, withElement: New ): Head *: TailRes =
            elems.head *: replacer.replace( elems.tail, withElement )
    }
}

object Replacer extends LowPriorityReplacers {
    type Aux[ I, R <: Tuple, New, O ] = Replacer[ I, R, New ] { type Out = O }

    given replaceHead[ Head, Tail <: Tuple, New ] : Replacer[ Nat._0, Head *: Tail, New ] with {
        type Out = New *: Tail

        override def replace( elems: Head *: Tail, withElement: New ) : Out =
            val tail = elems.tail
            withElement *: tail
    }

    given equiv[ I <: Int, N <: Nat, R <: Tuple, New, Res <: Tuple ](
        using
        ev : Nat.IntA[ I, N ],
        rp : Replacer.Aux[ N, R, New, Res ],
    ) : Replacer[ I, R, New ] with {
        type Out = Res

        override def replace( elems: R, withElement: New ) : Res =
            rp.replace( elems, withElement )
    }

    sealed class RP[ I ] {
        def apply[ R <: Tuple, Elem ](
            elems : R,
            withElem : Elem,
        )(
            using
            rp : Replacer[ I, R, Elem ],
        ) : rp.Out = rp.replace( elems, withElem )
    }

    def replace[ I ] : RP[ I ] = new RP[ I ]
}

trait Retriever[ I, R <: Tuple ] {
    type Out

    def retrieve( from : R ) : Out
}

trait LowPriorityRetrievers {
    given continue[ I <: Nat, DecI <: Nat, Head, Tail <: Tuple, TailRes ](
        using
        ev : Nat.DecA[ I, DecI ],
        next : Retriever.Aux[ DecI, Tail, TailRes ],
    ) : Retriever[ I, Head *: Tail ] with {
        type Out = TailRes

        override def retrieve( from: Head *: Tail ): TailRes =
            next.retrieve( from.tail )
    }
}

object Retriever extends LowPriorityRetrievers {
    type Aux[ I, R <: Tuple, O ] = Retriever[ I, R ] { type Out = O }

    given retrieveHead[ Head, Tail <: Tuple ] : Retriever[ Nat._0, Head *: Tail ] with {
        type Out = Head

        override def retrieve( from: Head *: Tail ) : Out = from.head
    }

    given equiv[ I <: Int, N <: Nat, R <: Tuple, Res ](
        using
        ev : Nat.IntA[ I, N ],
        rt : Retriever.Aux[ N, R, Res ],
    ) : Retriever[ I, R ] with {
        type Out = Res

        override def retrieve( from: R ) : Out = rt.retrieve( from )
    }

    sealed class RT[ I ] {
        def apply[ R <: Tuple ](
            from : R,
        )(
            using
            rt : Retriever[ I, R ],
        ) : rt.Out = rt.retrieve( from )
    }

    def retrieve[ I ] : RT[ I ] = new RT[ I ]
}

trait TypeNames[ T <: Tuple ] {
    def labels : List[ String ]
}

object TypeNames {
    given empty : TypeNames[ EmptyTuple ] with {
        def labels : List[ String ] = Nil
    }

    given tup[ Head, Tail <: Tuple ](
        using
        mir : Mirror.Of[ Head ],
        hLab : ValueOf[ mir.MirroredLabel ],
        next : TypeNames[ Tail ],
    ) : TypeNames[ Head *: Tail ] with {
        def labels : List[ String ] = hLab.value :: next.labels
    }
}

