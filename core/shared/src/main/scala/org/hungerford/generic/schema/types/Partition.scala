package org.hungerford.generic.schema.types

import scala.util.NotGiven

trait Partition[ Condition[ _ ], R <: Tuple ] {
    type MeetsCondition <: Tuple
    type FailsCondition <: Tuple

    def filter( elems : R ) : (MeetsCondition, FailsCondition)
}

object Partition {
    type Aux[ C[ _ ], R <: Tuple, M <: Tuple, F <: Tuple ] = Partition[ C, R ] { type MeetsCondition = M; type FailsCondition = F }

    given empty[ C[ _ ] ] : Partition[ C, EmptyTuple ] with {
        override type MeetsCondition = EmptyTuple
        override type FailsCondition = EmptyTuple

        override def filter(
            elems : EmptyTuple
        ): (MeetsCondition, FailsCondition) = (EmptyTuple, EmptyTuple)
    }

    given succeeds[ C[ _ ], H, Tail <: Tuple, NextM <: Tuple, NextF <: Tuple ](
        using
        h : C[ H ],
        t : Partition.Aux[ C, Tail, NextM, NextF ],
    ) : Partition[ C, H *: Tail ] with {
        override type MeetsCondition = H *: NextM
        override type FailsCondition = NextF

        override def filter(
            elems : H *: Tail
        ): (MeetsCondition, FailsCondition) = {
            val (nextM, nextF) = t.filter( elems.tail )
            (elems.head *: nextM, nextF)
        }
    }

    given fails[ C[ _ ], H, Tail <: Tuple, NextM <: Tuple, NextF <: Tuple ](
        using
        h : NotGiven[ C[ H ] ],
        t : Partition.Aux[ C, Tail, NextM, NextF ],
    ) : Partition[ C, H *: Tail ] with {
        override type MeetsCondition = NextM
        override type FailsCondition = H *: NextF

        override def filter(
            elems : H *: Tail
        ): (MeetsCondition, FailsCondition) = {
            val (nextM, nextF) = t.filter( elems.tail )
            (nextM, elems.head *: nextF)
        }
    }
}
