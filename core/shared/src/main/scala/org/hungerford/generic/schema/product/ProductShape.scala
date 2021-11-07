package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldNamesCollector}
import org.hungerford.generic.schema.validator.Validator
import shapeless._
import shapeless.ops.hlist.{ToList, Tupler}

import scala.language.higherKinds


case class ProductShape[ T, Rt <: HList, RVt <: HList, AFt, AFSt , Tupt ](
    fieldDescriptions : Rt,
    additionalFieldsSchema : Schema.Aux[ AFt, AFSt ],
    private[ schema ] val constructor : (RVt, Map[ String, AFt ]) => T,
    private[ schema ] val deconstructor : T => (RVt, Map[ String, AFt ])
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, Rt, RVt ],
    val tupler : Tupler.Aux[ RVt, Tupt ],
    lengther : HListIntLength[ Rt ],
    fns : FieldNamesCollector[ Rt ],
) {

    // Field descriptions
    type R = Rt

    type RV = RVt

    type AF = AFt

    type AFS = AFSt

    type Tup = Tupt

    def construct( fieldParams : Tup, additionalFields : Map[ String, AF ] )(
        implicit tupleGeneric : Generic.Aux[ Tup, RVt ],
    ) : T =
        constructor( tupleGeneric.to( fieldParams ), additionalFields )

    def deconstruct( value : T ) : (Tup, Map[ String, AF ]) = {
        val (fieldsRVt, additionalFields) = deconstructor( value )
        (fieldsRVt.tupled, additionalFields)
    }

    lazy val size : Int = lengther.length

    def fields : Set[ String ] = fns.collect( fieldDescriptions )
}


trait HListIntLength[ L <: HList ] {
    def length : Int
}

object HListIntLength {
    def apply[ L <: HList ]( implicit ev : HListIntLength[ L ] ) : HListIntLength[ L ] = ev

    implicit def hnilLength : HListIntLength[ HNil ] = new HListIntLength[HNil] {
        override def length : Int = 0
    }

    implicit def generalLength[ T, Tail <: HList ]( implicit ev : HListIntLength[ Tail ] ) : HListIntLength[ T :: Tail ] = {
        new HListIntLength[ T :: Tail] {
            override def length : Int = 1 + ev.length
        }
    }
}

/**
 * Type class witness that every element in one hlist has type of the corresponding
 * element in another hlist wrapped in some context type.
 * E.g., List[Int] :: List[Double] :: List[String] :: HNil corresponds to Int :: Double :: String :: HNil
 * @tparam F context type
 * @tparam Rt hlist of context types
 * @tparam RVt hlist of value types
 */
sealed trait CtxWrapHListsConstraint[ F[ _ ], Rt <: HList, RVt <: HList ]

object CtxWrapHListsConstraint {

    implicit def hnilFieldAndDescConst[ F[ _ ] ] : CtxWrapHListsConstraint[ F, HNil, HNil ] = new CtxWrapHListsConstraint[ F, HNil, HNil ] {}

    implicit def idFieldAndDescConst[ F[ _ ], HeadRt, TailRt <: HList, HeadRVt, TailRVt <: HList ](
        implicit
        evHead : HeadRt <:< F[ HeadRVt ],
        evNotNothing : HeadRt =:!= Nothing,
        evTail : CtxWrapHListsConstraint[ F, TailRt, TailRVt ],
    ) : CtxWrapHListsConstraint[ F, HeadRt :: TailRt, HeadRVt :: TailRVt ] = new CtxWrapHListsConstraint[ F, HeadRt :: TailRt, HeadRVt :: TailRVt ] {}

    def apply[ F[ _ ], Rt <: HList, RVt <: HList ]( implicit ev : CtxWrapHListsConstraint[ F, Rt, RVt ] ) : CtxWrapHListsConstraint[ F, Rt, RVt ] = ev

}
