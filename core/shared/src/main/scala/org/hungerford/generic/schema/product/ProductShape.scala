package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.product.field.{FieldDescription, UniqueFieldNames}
import org.hungerford.generic.schema.validator.Validator

import scala.language.higherKinds
import org.hungerford.generic.schema.product.field.FieldDescriptionCase


case class ProductShape[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt, C, DC ](
    fieldDescriptions : Rt,
    additionalFieldsSchema : Schema.Aux[ AFt, AFSt ],
    private[ schema ] val constructor : C,
    private[ schema ] val deconstructor : T => DC,
)(
    using
    fieldsConstraint : CtxWrapTuplesConstraint[ FieldDescription, Rt, RVt ],
    uniqueFields : UniqueFieldNames[ Rt ],
    lengther : TupleIntLength[ Rt ],
    prodConst : ProductConstructor[ C, RVt, AFt, T ],
    prodDeconst : ProductDeconstructor[ T, RVt, AFt, DC ],
) {

    // Field descriptions
    type R = Rt

    type RV = RVt

    type AF = AFt

    type AFS = AFSt

    type Cons = C

    type Decons = DC

    def construct : C = constructor

    def deconstruct( value : T ) : DC = {
        deconstructor( value )
    }

    lazy val size : Int = fieldDescriptions.size

    lazy val fieldNames : Set[ String ] = {
        def getFieldNames[ FDs <: Tuple ]( fds : FDs, fns : Set[ String ] = Set.empty ) : Set[ String ] = {
            fds match {
                case EmptyTuple => fns
                case FieldDescriptionCase( name, _, _, _ ) *: next =>
                    getFieldNames( next, fns + name )
            }
        }
        getFieldNames( fieldDescriptions, Set.empty )
    }
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

    given hnilFieldAndDescConst[ F[ _ ] ] : CtxWrapTuplesConstraint[ F, EmptyTuple, EmptyTuple ] = new CtxWrapTuplesConstraint[ F, EmptyTuple, EmptyTuple ] {}

    given idFieldAndDescConst[ F[ _ ], HeadRt, TailRt <: Tuple, HeadRVt, TailRVt <: Tuple ](
        using
        evHead : => HeadRt <:< F[ HeadRVt ],
        evTail : CtxWrapTuplesConstraint[ F, TailRt, TailRVt ],
    ) : CtxWrapTuplesConstraint[ F, HeadRt *: TailRt, HeadRVt *: TailRVt ] = new CtxWrapTuplesConstraint[ F, HeadRt *: TailRt, HeadRVt *: TailRVt ] {}

    def apply[ F[ _ ], Rt <: Tuple, RVt <: Tuple ]( implicit ev : CtxWrapTuplesConstraint[ F, Rt, RVt ] ) : CtxWrapTuplesConstraint[ F, Rt, RVt ] = ev

}
