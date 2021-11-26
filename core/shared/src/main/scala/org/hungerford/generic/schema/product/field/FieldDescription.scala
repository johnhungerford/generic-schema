package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.FieldDescription.Aux
import org.hungerford.generic.schema.validator.Validator

import scala.collection.mutable
import scala.language.higherKinds
import scala.annotation.meta.field

import scala.compiletime.{erasedValue, summonInline}
import org.hungerford.generic.schema.types.SimpleExtractor

type FieldName = Stringleton

trait FieldDescription[ T ] {
    type Name <: FieldName
    type Shape

    val fieldName : Name
    val schema : Schema.Aux[ T, Shape ]
    val description : Option[ String ] = None
    val validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ]
}

object FieldDescription {
    type Aux[ T, N <: FieldName, S ] = FieldDescription[ T ] { type Name = N; type Shape = S }
    type AuxN[ T, N <: FieldName ] = FieldDescription[ T ] { type Name = N }
    type AuxS[ T, S ] = FieldDescription[ T ] { type Shape = S }
}

case class FieldDescriptionCase[ T, N <: FieldName, S ](
    override val fieldName : N,
    override val schema : Schema.Aux[ T, S ],
    override val description : Option[ String ] = None,
    override val validators : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) extends FieldDescription[ T ] {
    type Name = N
    type Shape = S
}
