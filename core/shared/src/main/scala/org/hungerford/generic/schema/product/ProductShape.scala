package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.product.field.{Field, FieldCase, FieldGetter, FieldName, UniqueFieldNames}
import org.hungerford.generic.schema.types.CtxWrapTuplesConstraint
import org.hungerford.generic.schema.validator.Validator

import scala.language.higherKinds


trait ValidAfExtr[ T, AF, AFE ]

object ValidAfExtr {
    given [ T, AF ] : ValidAfExtr[ T, AF, T => AF ] with {}
    given [ T ] : ValidAfExtr[ T, Nothing, Unit ] with {}
}

case class ProductShape[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt, AFEt, C ](
    fieldDescriptions : Rt,
    additionalFieldsSchema : Schema.Aux[ AFt, AFSt ],
    private[ schema ] val afExtractor : AFEt,
    private[ schema ] val constructor : C,
)(
    using
    fieldsConstraint : CtxWrapTuplesConstraint[ Field.Ctx[ T ], Rt, RVt ],
    uniqueFields : UniqueFieldNames[ Rt ],
    prodConst : ProductConstructor[ C, RVt, AFt, T ],
    afExtrEv : ValidAfExtr[ T, AFt, AFEt ],
) {

    // Field descriptions
    type R = Rt

    type RV = RVt

    type AF = AFt

    type AFS = AFSt

    type AFE = AFEt

    type Cons = C

    lazy val size : Int = fieldDescriptions.size

    lazy val fieldNames : Set[ String ] = {
        def getFieldNames[ FDs <: Tuple ]( fds : FDs, fns : Set[ String ] = Set.empty ) : Set[ String ] = {
            fds match {
                case EmptyTuple => fns
                case FieldCase( name, _, _, _, _, _, _, _ ) *: next =>
                    getFieldNames( next, fns + name )
                case _ => ???
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

    def construct : C = constructor

    def extractNamedFields(
        value : T,
    )(
        using
        decons : ProductDeconstructor[ T, R ]
    ) : decons.Res = decons.deconstruct( value, fieldDescriptions )

    def extractAdditionalFields(
        value : T,
    )(
        using
        ev : AFE =:= (T => AF)
    ) : AF = ev( afExtractor )( value )

    def extract(
        value : T,
    )(
        using
        decons : ProductDeconstructor[ T, (AFE, R) ],
    ) : decons.Res = decons.deconstruct( value, (afExtractor, fieldDescriptions) )
}
