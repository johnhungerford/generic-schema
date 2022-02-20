package org.hungerford.generic.schema.coproduct.subtype

import org.hungerford.generic.schema.coproduct.subtype
import org.hungerford.generic.schema.selector.TypeSelector
import org.hungerford.generic.schema.types.{Nat, Retriever}

import scala.util.NotGiven

trait SubtypeRetriever[ N <: Singleton, R <: Tuple ] {
    type Subtype

    def retrieve( from : R ) : Subtype
}

trait LowPrioritySubtypeRetrievers {
    given headDoesNotHaveName[ N <: TypeName, Head, Tail <: Tuple, Next ](
        using
        next : SubtypeRetriever.Aux[ N, Tail, Next ],
    ) : SubtypeRetriever.Aux[ N, Head *: Tail, Next ] = {
        new SubtypeRetriever[ N, Head *: Tail ] {
            type Subtype = next.Subtype

            override def retrieve( from : Head *: Tail ) : Next =
                next.retrieve( from.tail )
        }
    }
}

object SubtypeRetriever extends LowPrioritySubtypeRetrievers {
    type Aux[ N <: Singleton, R <: Tuple, ST ] = SubtypeRetriever[ N, R ] { type Subtype = ST }

    def apply[ N <: Singleton, R <: Tuple ](
        using
        str : SubtypeRetriever[ N, R ],
    ) : SubtypeRetriever.Aux[ N, R, str.Subtype ] = str

    given headHasName[ N <: TypeName, SubT, Tail <: Tuple ](
        using
        stn : SubtypeOfName[ N, SubT ],
    ) : SubtypeRetriever.Aux[ N, SubT *: Tail, SubT ] = {
        new SubtypeRetriever[ N, SubT *: Tail ] {
            type Subtype = SubT

            override def retrieve( from : SubT *: Tail ) : SubT = from.head
        }
    }

    given retrieverByIndex[ I <: Int & Singleton, N <: Nat, R <: Tuple, Elem ](
        using
        ev : Nat.IntA[ I, N ],
        rt : Retriever.Aux[ N, R, Elem ],
    ) : SubtypeRetriever[ I, R ] with {
        type Subtype = Elem

        override def retrieve( from: R ): Subtype = rt.retrieve( from )

    }

    def retrieve[ N <: Singleton, R <: Tuple ](
        typeName : N,
        subtypes : R,
    )(
        using
        fr : SubtypeRetriever[ N, R ],
    ) : fr.Subtype = fr.retrieve( subtypes )
}

trait SubtypeTypeRetriever[ T, N <: Nat, R <: Tuple ] {
    type Subtype

    def retrieve( from : R ) : Subtype
}

trait LowPrioritySubtypeTypeRetrievers {}

object SubtypeTypeRetriever {


    def retrieve[ T, N <: Nat, R <: Tuple ](
        sel : TypeSelector[ T, N ],
        from : R,
    )(
        using
        rt : SubtypeTypeRetriever[ T, N, R ]
    ) : rt.Subtype = rt.retrieve( from )
}
