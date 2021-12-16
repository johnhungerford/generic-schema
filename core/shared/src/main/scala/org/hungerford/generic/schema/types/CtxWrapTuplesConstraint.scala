package org.hungerford.generic.schema.types

/**
 * Type class witness that every element in one hlist has type of the corresponding
 * element in another hlist wrapped in some context type.
 * E.g., List[Int] *: List[Double] *: List[String] *: EmptyTuple corresponds to Int *: Double *: String *: EmptyTuple
 *
 * @tparam F   context type
 * @tparam Rt  hlist of context types
 * @tparam RVt hlist of value types
 */
sealed trait CtxWrapTuplesConstraint[ F[ _ ], Rt <: Tuple, RVt <: Tuple ]

object CtxWrapTuplesConstraint {

  given hnilFieldAndDescConst[ F[ _ ] ]: CtxWrapTuplesConstraint[ F, EmptyTuple, EmptyTuple ] = new 
      CtxWrapTuplesConstraint[ F, EmptyTuple, EmptyTuple ] {}

  given idFieldAndDescConst[ F[ _ ], HeadRt, TailRt <: Tuple, HeadRVt, TailRVt <: Tuple ](
    using
    evHead: => HeadRt <:< F[ HeadRVt ],
    evTail: CtxWrapTuplesConstraint[ F, TailRt, TailRVt ],
  ): CtxWrapTuplesConstraint[ F, HeadRt *: TailRt, HeadRVt *: TailRVt ] = new CtxWrapTuplesConstraint[ F, HeadRt 
    *: TailRt, HeadRVt *: TailRVt ] {}

  def apply[ F[ _ ], Rt <: Tuple, RVt <: Tuple ](
    implicit ev: CtxWrapTuplesConstraint[ F, Rt, RVt ]
  ): CtxWrapTuplesConstraint[ F, Rt, RVt ] = ev

}