package org.hungerford.generic.schema.types

import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.ProductDeconstructor
import org.hungerford.generic.schema.product.field.{Field, FieldName}
import org.hungerford.generic.schema.types.Validation.validate

trait Validation[T, C] {
    def isValid(value: T, informedBy : C): Boolean
}

trait HasValidate {
    protected def validate[T](value: T, validators: Set[Validator[T]]): Boolean =
        validators.forall( nextValidator => nextValidator.isValid( value ) )
}

trait Validation1 extends HasValidate {
    given genericValidation[T, S](
        using
        shapeValidation : Validation[T, S],
    ) : Validation[T, Schema.Aux[T, S]] with {
        override def isValid( value: T, informedBy: Schema.Aux[T, S] ): Boolean =
            shapeValidation.isValid(value, informedBy.shape) && validate(value, informedBy.genericValidators)
    }

    given emptySubtypesValidation[T] : Validation[T, EmptyTuple] with {
        override def isValid( value: T, informedBy: EmptyTuple ): Boolean = true
    }
}

object Validation extends Validation1 {
    given primitiveValidation[ T ] : Validation[T, Schema.Aux[T, Unit]] with {
        def isValid( value: T, informedBy: Schema.Aux[T, Unit] ): Boolean = validate(value, informedBy.genericValidators)
    }

    given noSchemaValidation[ T ] : Validation[T, Schema.Aux[T, Nothing]] with {
        def isValid( value: T, informedBy: Schema.Aux[T, Nothing] ): Boolean = validate(value, informedBy.genericValidators)
    }

    given productValidationAf[T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C](
        using
        fieldsValidation : Validation[RV, R],
        afValidation: Validation[AF, Schema.Aux[AF, AFS]],
        prodDec: ProductDeconstructor.Aux[T, (AFE, R), (Map[String, AF], RV)],
    ): Validation[T, ProductShape[T, R, RV, AF, AFS, AFE, C]] with {
        override def isValid(
            value: T,
            informedBy: ProductShape[ T, R, RV, AF, AFS, AFE, C ]
        ): Boolean =
            val (afVs, fieldVs) = prodDec.deconstruct(value, (informedBy.afExtractor, informedBy.fieldDescriptions))
            val fieldsAreValid = fieldsValidation.isValid(fieldVs, informedBy.fieldDescriptions)
            val afsAreValid = afVs.forall(afTup => afValidation.isValid(afTup._2, informedBy.additionalFieldsSchema))
            fieldsAreValid && afsAreValid
    }

    given productValidationNoAf[T, R <: Tuple, RV <: Tuple, C](
        using
        fieldsValidation : Validation[RV, R],
        prodDec: ProductDeconstructor.Aux[T, R, RV],
    ): Validation[T, ProductShape[T, R, RV, Nothing, Unit, Unit, C]] with {
        override def isValid(
            value: T,
            informedBy: ProductShape[ T, R, RV, Nothing, Unit, Unit, C ]
        ): Boolean =
            val fieldVs = prodDec.deconstruct(value, informedBy.fieldDescriptions)
            val fieldsAreValid = fieldsValidation.isValid(fieldVs, informedBy.fieldDescriptions)
            fieldsAreValid
    }

    given fieldsValidation[RVHead, RVTail <: Tuple, RHead, RTail <: Tuple](
        using
        headValid : Validation[RVHead, RHead],
        tailValid : Validation[RVTail, RTail],
    ) : Validation[RVHead *: RVTail, RHead *: RTail] with {
        override def isValid(
            value: RVHead *: RVTail,
            informedBy: RHead *: RTail
        ): Boolean =
            headValid.isValid(value.head, informedBy.head) &&
              tailValid.isValid(value.tail, informedBy.tail)
    }

    given emptyFieldsValidation : Validation[EmptyTuple, EmptyTuple] with {
        override def isValid(
            value: EmptyTuple,
            informedBy: EmptyTuple,
        ): Boolean = true
    }

    given fieldValidation[T, F, N <: FieldName, S](
        using
        fieldSchemaValid : Validation[F, Schema.Aux[F, S]],
    ) : Validation[F, Field[T, F, N, S]] with {
        override def isValid(
            value: F,
            informedBy: Field[ T, F, N, S ]
        ): Boolean =
            validate(value, informedBy.validators) &&
              fieldSchemaValid.isValid(value, informedBy.schema)
    }

    given coproductValidation[T, R <: Tuple, RV <: Tuple, D, DN](
        using
        valid : Validation[T, R],
    ) : Validation[T, CoproductShape[T, R, RV, D, DN]] with {
        override def isValid(
            value: T,
            informedBy: CoproductShape[ T, R, RV, D, DN ]
        ): Boolean = valid.isValid(value, informedBy.subtypeDescriptions)
    }

    given subtypesValidation[T, ST, D, DN, DV, N <: TypeName, STS, RTail <: Tuple](
        using
        headValid : Validation[ST, Schema.Aux[ST, STS]],
        tailValid : Validation[T, RTail],
    ) : Validation[T, Subtype[T, ST, D, DN, DV, N, STS] *: RTail] with {
        override def isValid( value: T, informedBy: Subtype[T, ST, D, DN, DV, N, STS] *: RTail ): Boolean =
            val subtype = informedBy.head
            val nextSubtypes = informedBy.tail
            subtype.fromSuper(value) match {
                case None => tailValid.isValid(value, informedBy.tail)
                case Some(st) =>
                    validate(st, subtype.validators) &&
                      headValid.isValid(st, subtype.schema)
            }
    }

    given subtypeValidation[T, ST, D, DN, DV, N <: TypeName, STS](
        using
        schVal : Validation[ST, Schema.Aux[ST, STS]],
    ): Validation[ST, Subtype[T, ST, D, DN, DV, N, STS]] with {
        override def isValid(
            value: ST,
            informedBy: Subtype[ T, ST, D, DN, DV, N, STS ]
        ): Boolean =
            validate(value, informedBy.validators) &&
              schVal.isValid(value, informedBy.schema)
    }
}
