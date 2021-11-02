package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.FieldDescription.Aux
import org.hungerford.generic.schema.{PrimitiveSchemaBuilder, Schema, SchemaBuilder}
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder, FieldDescriptionBuilderWithoutSchema}
import org.hungerford.generic.schema.validator.Validator
import shapeless.ops.hlist.{Prepend, Tupler}
import shapeless._

case class ProductSchemaBuilder[ T, R <: HList, RV <: HList, AF, AFS <: Schema[ AF ] ](
    private[ product ] val desc : Option[ String ] = None,
    private[ product ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private[ product ] val aftSch : Option[ AFS ] = None,
    private[ product ] val fieldDescs : R,
)(
    implicit
    fieldsConstraint : CtxWrapHListsConstraint[ FieldDescription, R, RV ],
    val tupler : Tupler[ RV ],
) {
    def description( description : String ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] = copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : ProductSchemaBuilder[ T, R, RV, AF, AFS ] = copy( vals = validators.toSet )

    def addField[ F ](
        builder : FieldDescriptionBuilderWithoutSchema[ F ] => FieldDescription[ F ],
    )(
        implicit tup : Tupler[ F :: RV ],
    ) : ProductSchemaBuilder[ T, FieldDescription[ F ] :: R, F :: RV, AF, AFS ] = {
        val fd : FieldDescription[ F ] = builder( FieldDescriptionBuilder[ F ] )
        val newFieldDescs = fd :: fieldDescs
        copy[ T, FieldDescription[ F ] :: R, F :: RV, AF, AFS ]( desc, vals, aftSch, newFieldDescs )
    }

    def additionalFields[ F, S <: Schema[ F ] ]( implicit schema : S ) : ProductSchemaBuilder[ T, R, RV, F, S ] = {
        copy[ T, R, RV, F, S ]( desc, vals, Some( schema ), fieldDescs )
    }

    def buildAdditionalFieldsSchema[ F, S <: Schema[ F ] ]( builder : SchemaBuilder[ F ] => S ) : ProductSchemaBuilder[ T, R, RV, F, S ] = {
        copy[ T, R, RV, F, S ]( desc, vals, Some( builder( SchemaBuilder[ F ] ) ) )
    }

//    def constructor()
}
