package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.ProductSchemaBuilder
import org.hungerford.generic.schema.validator.Validator

case class SchemaBuilder[ T ](
    private val desc : Option[ String ] = None,
    private val vals : Set[ Validator[ T ] ] = Set.empty,
) {
    def description( description : String ) : SchemaBuilder[ T ] = copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : SchemaBuilder[ T ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : SchemaBuilder[ T ] = copy( vals = validators.toSet )

    def primitive : PrimitiveSchemaBuilder[ T ] = PrimitiveSchemaBuilder[ T ]( desc, vals )
    def buildPrimitive : Primitive[ T ] = Primitive[ T ]( desc, vals )
    def product : ProductSchemaBuilder[ T ] = ProductSchemaBuilder[ T ]( desc, vals )
}

case class PrimitiveSchemaBuilder[ T ](
    private val desc : Option[ String ] = None,
    private val vals : Set[ Validator[ T ] ] = Set.empty,
) {
    def description( description : String ) : PrimitiveSchemaBuilder[ T ] = copy( desc = Some( description ) )
    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : PrimitiveSchemaBuilder[ T ] =
        validate ( validator +: otherValidators )
    def validate( validators : Iterable[ Validator[ T ] ] ) : PrimitiveSchemaBuilder[ T ] = copy( vals = validators.toSet )

    def build : Primitive[ T ] = Primitive[ T ](
        desc,
        vals,
    )
}

object SchemaBuilder {
    def apply[ T ] : SchemaBuilder[ T ] = SchemaBuilder[ T ]()
    def none : NoSchema.type = NoSchema
}
