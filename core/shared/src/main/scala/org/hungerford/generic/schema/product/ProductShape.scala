package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.product.field.{Field, FieldCase, FieldGetter, FieldName, UniqueFieldNames}
import org.hungerford.generic.schema.types.CtxWrapTuplesConstraint
import org.hungerford.generic.schema.validator.Validator

import scala.language.higherKinds


case class ProductShape[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt, C, DC ](
    fieldDescriptions : Rt,
    additionalFieldsSchema : Schema.Aux[ AFt, AFSt ],
    private[ schema ] val constructor : C,
    private[ schema ] val deconstructor : DC,
)(
    using
    fieldsConstraint : CtxWrapTuplesConstraint[ Field, Rt, RVt ],
    uniqueFields : UniqueFieldNames[ Rt ],
    lengther : TupleIntLength[ Rt ],
    prodConst : ProductConstructor[ C, RVt, AFt, T ],
    prodDeconst : ProductDeconstructor[ DC, RVt, AFt, T ],
) {

    // Field descriptions
    type R = Rt

    type RV = RVt

    type AF = AFt

    type AFS = AFSt

    type Cons = C

    type Decons = DC

    def construct : C = constructor

    def deconstruct : DC = deconstructor

    lazy val size : Int = fieldDescriptions.size

    lazy val fieldNames : Set[ String ] = {
        def getFieldNames[ FDs <: Tuple ]( fds : FDs, fns : Set[ String ] = Set.empty ) : Set[ String ] = {
            fds match {
                case EmptyTuple => fns
                case FieldCase( name, _, _, _, _, _, _ ) *: next =>
                    getFieldNames( next, fns + name )
            }
        }
        getFieldNames( fieldDescriptions, Set.empty )
    }

    def getField[ N <: FieldName ](
        fieldName : N,
        from : T,
    )(
        using
        fg : FieldGetter[ N, R, RV ],
    ) : fg.Out = fg.get( prodDeconst.deconstruct( deconstructor )( from )._1 )
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




