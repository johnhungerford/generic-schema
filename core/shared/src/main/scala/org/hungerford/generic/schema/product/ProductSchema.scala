package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.field.FieldDescription
import org.hungerford.generic.schema.validator.Validator
import shapeless._
import shapeless.ops.hlist.Tupler

import scala.annotation.implicitNotFound
import scala.language.higherKinds


case class ProductSchema[ T, Rt <: HList, RVt <: HList, AFt, AFtRt ](
    genericDescription : Option[ String ],
    genericValidators : Set[ Validator[ T ] ],
    fieldDescriptions : Rt,
    additionalFieldsSchema : Schema.Aux[ AFt, AFtRt ],
    private[ schema ] val constructor : (RVt, Map[ String, AFt ]) => T,
    private[ schema ] val deconstructor : T => (RVt, Map[ String, AFt ])
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, Rt, RVt ],
    val tupler : Tupler[ RVt ],
    lengther : HListIntLength[ Rt ],
    generic: Generic.Aux[ T, RVt ],
) extends Schema[ T ] {
    // Field descriptions
    override type R = Rt

    type RV = RVt

    type AdditionalFieldType = AFt

    def construct( fieldParams : tupler.Out, additionalFields : Map[ String, AdditionalFieldType ] )(
        implicit tupleGeneric : Generic.Aux[ tupler.Out, RVt ],
    ) : T =
        constructor( tupleGeneric.to( fieldParams ), additionalFields )

    def deconstruct( value : T ) : (Tupler[ RVt ]#Out, Map[ String, AdditionalFieldType ]) = {
        val (fieldsRVt, additionalFields) = deconstructor( value )
        (fieldsRVt.tupled, additionalFields)
    }

    lazy val size : Int = lengther.length

    override def withDescription( description : String ) : Schema[ T ] = copy( genericDescription = Some( description ) )

    override def withoutDescription : Schema[ T ] = copy( genericDescription = None )

    override def withValidation( validators : Validator[ T ]* ) : Schema[ T ] = copy( genericValidators = genericValidators ++ validators.toSet )

    override def withoutValidation : Schema[ T ] = copy( genericValidators = Set.empty )
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
        evTail : CtxWrapHListsConstraint[ F, TailRt, TailRVt ],
    ) : CtxWrapHListsConstraint[ F, HeadRt :: TailRt, HeadRVt :: TailRVt ] = new CtxWrapHListsConstraint[ F, HeadRt :: TailRt, HeadRVt :: TailRVt ] {}

    def apply[ F[ _ ], Rt <: HList, RVt <: HList ]( implicit ev : CtxWrapHListsConstraint[ F, Rt, RVt ] ) : CtxWrapHListsConstraint[ F, Rt, RVt ] = ev

}
