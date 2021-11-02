package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.ProductSchemaBuilder
import org.hungerford.generic.schema.validator.Validator
import shapeless._
import shapeless.ops.hlist.Tupler

case class SchemaBuilder[ T ](
    private[ schema ] val desc : Option[ String ] = None,
    private[ schema ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) {
    def description( description : String ) : SchemaBuilder[ T ] = copy( desc = Some( description ) )

    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : SchemaBuilder[ T ] =
        validate( validator +: otherValidators )

    def validate( validators : Iterable[ Validator[ T ] ] ) : SchemaBuilder[ T ] = copy( vals = validators.toSet )

    def primitive : PrimitiveSchemaBuilder[ T ] = PrimitiveSchemaBuilder[ T ]( desc, vals )

    def buildPrimitive : Primitive[ T ] = Primitive[ T ]( desc, vals )

    def product : ProductSchemaBuilder[ T, HNil, HNil, Nothing, NoSchema.type, Unit ] =
        ProductSchemaBuilder[ T, HNil, HNil, Nothing, NoSchema.type, Unit ](
            desc,
            vals,
            NoSchema,
            HNil,
        )
}

case class PrimitiveSchemaBuilder[ T ](
    private[ schema ] val desc : Option[ String ] = None,
    private[ schema ] val vals : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
) {
    def description( description : String ) : PrimitiveSchemaBuilder[ T ] = copy( desc = Some( description ) )

    def validate( validator : Validator[ T ], otherValidators : Validator[ T ]* ) : PrimitiveSchemaBuilder[ T ] =
        validate( validator +: otherValidators )

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
