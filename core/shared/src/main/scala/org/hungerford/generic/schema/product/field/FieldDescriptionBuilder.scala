package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{Schema, SchemaBuilder}


case class FieldDescriptionBuilderWithoutSchema[ T ](
    private val fn : Option[ String ] = None,
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
){
    def fromSchema[ S <: Schema[ T ] ]( implicit schema : S ) : FieldDescriptionBuilderWithSchema[ T, S ] = {
        FieldDescriptionBuilderWithSchema[ T, S ](
            schema,
            fn,
            desc,
            vs,
        )
    }

    def buildSchema[ Rt, S <: Schema[ T ] ]( builder : SchemaBuilder[ T ] => S ) : FieldDescriptionBuilderWithSchema[ T, S ] = {
        fromSchema( builder( SchemaBuilder[ T ] ) )
    }

    def fieldName( name : String ) : FieldDescriptionBuilderWithoutSchema[ T ] = copy( fn = Some( name ) )
    def description( description : String ) : FieldDescriptionBuilderWithoutSchema[ T ] = copy( desc = Some( description ) )
    def validate( validators : Validator[ T ]* ) : FieldDescriptionBuilderWithoutSchema[ T ] = copy( vs = validators.toSet )
}

case class FieldDescriptionBuilderWithSchema[ T, S <: Schema[ T ] ](
    private val sch : S,
    private val fn : Option[ String ] = None,
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ],
) {
    def fieldName( name : String ) : FieldDescriptionBuilderWithSchema[ T, S ] =
        copy( fn = Some( name ) )
    def description( description : String ) : FieldDescriptionBuilderWithSchema[ T, S ] =
        copy( desc = Some( description ) )
    def validate( validators : Validator[ T ]* ) : FieldDescriptionBuilderWithSchema[ T, S ] =
        copy( vs = validators.toSet )
}

object FieldDescriptionBuilder {
    def apply[ T ] : FieldDescriptionBuilderWithoutSchema[ T ] = FieldDescriptionBuilderWithoutSchema[ T ]()
}
