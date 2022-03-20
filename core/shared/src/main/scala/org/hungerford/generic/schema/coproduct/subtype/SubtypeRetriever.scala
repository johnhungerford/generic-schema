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

    given headHasName[ N <: TypeName, SubT <: Subtype.Named[ N ], Tail <: Tuple ] : SubtypeRetriever.Aux[ N, SubT *: Tail, SubT ] = {
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

trait LowPrioritySubtypeTypeRetrievers {
    given nextSubtypeTypeRetriever[ T, N <: Nat, Head, Tail <: Tuple, Next ](
        using
        next : SubtypeTypeRetriever.Aux[ T, N, Tail, Next ],
    ) : SubtypeTypeRetriever[ T, N, Head *: Tail ] with {
        type Subtype = Next

        override def retrieve( from: Head *: Tail ) : Subtype =
            next.retrieve( from.tail )
    }
}

object SubtypeTypeRetriever extends LowPrioritySubtypeTypeRetrievers {
    type Aux[ T, N <: Nat, R <: Tuple, ST ] = SubtypeTypeRetriever[ T, N, R ] { type Subtype = ST }

    given zero[ ST, SubT <: Subtype.Tpe[ ST ], Tail <: Tuple ] : SubtypeTypeRetriever[ ST, Nat._0, SubT *: Tail ] with {
        type Subtype = SubT
        override def retrieve( from: SubT *: Tail ) : SubT = from.head
    }

    given nonZero[ ST, N <: Nat, DecN <: Nat, SubT <: Subtype.Tpe[ ST ], Tail <: Tuple, Res ](
        using
        ev : Nat.DecA[ N, DecN ],
        next : SubtypeTypeRetriever.Aux[ ST, DecN, Tail, Res ],
    ) : SubtypeTypeRetriever[ ST, N, SubT *: Tail ] with {
        type Subtype = Res
        override def retrieve( from: SubT *: Tail ) : Res = next.retrieve( from.tail )
    }

    def retrieve[ T, N <: Nat, R <: Tuple ](
        sel : TypeSelector[ T, N ],
        from : R,
    )(
        using
        rt : SubtypeTypeRetriever[ T, N, R ]
    ) : rt.Subtype = rt.retrieve( from )
}
