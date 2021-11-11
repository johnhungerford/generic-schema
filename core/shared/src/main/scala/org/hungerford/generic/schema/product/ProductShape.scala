package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.field.FieldDescription
import org.hungerford.generic.schema.validator.Validator

import scala.language.higherKinds


case class ProductShape[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt ](
    fieldDescriptions : Rt,
    additionalFieldsSchema : Schema.Aux[ AFt, AFSt ],
    private[ schema ] val constructor : (RVt, Map[ String, AFt ]) => T,
    private[ schema ] val deconstructor : T => (RVt, Map[ String, AFt ])
)(
    implicit
    fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, Rt, RVt ],
    lengther : TupleIntLength[ Rt ],
) {

    // Field descriptions
    type R = Rt

    type RV = RVt

    type AF = AFt

    type AFS = AFSt

    def construct( fieldParams : RV, additionalFields : Map[ String, AF ] ) : T =
        constructor( fieldParams, additionalFields )

    def deconstruct( value : T ) : (RV, Map[ String, AF ]) = {
        deconstructor( value )
    }


    lazy val size : Int = fieldDescriptions.size
}


trait TupleIntLength[ L <: Tuple ] {
    def length : Int
}

object TupleIntLength {
    def apply[ L <: Tuple ]( implicit ev : TupleIntLength[ L ] ) : TupleIntLength[ L ] = ev

    implicit def hnilLength : TupleIntLength[ EmptyTuple ] = new TupleIntLength[EmptyTuple] {
        override def length : Int = 0
    }

    implicit def generalLength[ T, Tail <: Tuple ]( implicit ev : TupleIntLength[ Tail ] ) : TupleIntLength[ T *: Tail ] = {
        new TupleIntLength[ T *: Tail] {
            override def length : Int = 1 + ev.length
        }
    }
}

/**
 * Type class witness that every element in one hlist has type of the corresponding
 * element in another hlist wrapped in some context type.
 * E.g., List[Int] *: List[Double] *: List[String] *: EmptyTuple corresponds to Int *: Double *: String *: EmptyTuple
 * @tparam F context type
 * @tparam Rt hlist of context types
 * @tparam RVt hlist of value types
 */
sealed trait CtxWrapTuplesConstraint[ F[ _ ], Rt <: Tuple, RVt <: Tuple ]

object CtxWrapTuplesConstraint {

    implicit def hnilFieldAndDescConst[ F[ _ ] ] : CtxWrapTuplesConstraint[ F, EmptyTuple, EmptyTuple ] = new CtxWrapTuplesConstraint[ F, EmptyTuple, EmptyTuple ] {}

    implicit def idFieldAndDescConst[ F[ _ ], HeadRt, TailRt <: Tuple, HeadRVt, TailRVt <: Tuple ](
        implicit
        evHead : HeadRt <:< F[ HeadRVt ],
        evTail : CtxWrapTuplesConstraint[ F, TailRt, TailRVt ],
    ) : CtxWrapTuplesConstraint[ F, HeadRt *: TailRt, HeadRVt *: TailRVt ] = new CtxWrapTuplesConstraint[ F, HeadRt *: TailRt, HeadRVt *: TailRVt ] {}

    def apply[ F[ _ ], Rt <: Tuple, RVt <: Tuple ]( implicit ev : CtxWrapTuplesConstraint[ F, Rt, RVt ] ) : CtxWrapTuplesConstraint[ F, Rt, RVt ] = ev

}
