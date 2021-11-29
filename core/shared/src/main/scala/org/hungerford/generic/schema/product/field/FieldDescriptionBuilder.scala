package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{Primitive, Schema, SchemaBuilder, SchemaRebuilder}


case class FieldDescriptionBuilderWithoutSchemaOrName[ T ](
   private val desc : Option[ String ] = None,
   private val vs : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
){
   def primitive : FieldDescriptionBuilderWithSchemaWithoutName[ T, Unit ] = {
       FieldDescriptionBuilderWithSchemaWithoutName[ T, Unit ](
           Primitive[ T ](),
           desc,
           vs,
       )
   }

   def fromSchema[ S ]( implicit schema : Schema.Aux[ T, S ] ) : FieldDescriptionBuilderWithSchemaWithoutName[ T, S ] = {
       FieldDescriptionBuilderWithSchemaWithoutName[ T, S ](
           schema,
           desc,
           vs,
       )
   }

   def buildSchema[ Rt, S ]( builder : SchemaBuilder[ T ] => Schema.Aux[ T, S ] ) : FieldDescriptionBuilderWithSchemaWithoutName[ T, S ] = {
       fromSchema( builder( SchemaBuilder[ T ] ) )
   }

   def fieldName[ N <: FieldName ]( name : N ) : FieldDescriptionBuilderWithoutSchemaWithName[ T, N ] =
       FieldDescriptionBuilderWithoutSchemaWithName[ T, N ]( name, desc, vs )
   def description( description : String ) : FieldDescriptionBuilderWithoutSchemaOrName[ T ] = copy( desc = Some( description ) )
   def validate( validators : Validator[ T ]* ) : FieldDescriptionBuilderWithoutSchemaOrName[ T ] = copy( vs = validators.toSet )
}

case class FieldDescriptionBuilderWithoutSchemaWithName[ T, N <: FieldName ](
    private val fn : N,
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
){
   def primitive : BuildableFieldDescriptionBuilder[ T, N, Unit ] = {
       BuildableFieldDescriptionBuilder[ T, N, Unit ](
           Primitive[ T ](),
           fn,
           desc,
           vs,
       )
   }

   def fromSchema[ S ]( implicit schema : Schema.Aux[ T, S ] ) : BuildableFieldDescriptionBuilder[ T, N, S ] = {
       BuildableFieldDescriptionBuilder[ T, N, S ](
           schema,
           fn,
           desc,
           vs,
       )
   }

   def buildSchema[ Rt, S ]( builder : SchemaBuilder[ T ] => Schema.Aux[ T, S ] ) : BuildableFieldDescriptionBuilder[ T, N, S ] = {
       fromSchema( builder( SchemaBuilder[ T ] ) )
   }

   def fieldName[ NewN <: FieldName ]( name : NewN ) : FieldDescriptionBuilderWithoutSchemaWithName[ T, NewN ] = copy[ T, NewN ]( fn = name )
   def description( description : String ) : FieldDescriptionBuilderWithoutSchemaWithName[ T, N ] = copy( desc = Some( description ) )
   def validate( validators : Validator[ T ]* ) : FieldDescriptionBuilderWithoutSchemaWithName[ T, N ] = copy( vs = validators.toSet )
}

case class FieldDescriptionBuilderWithSchemaWithoutName[ T, S ](
    private val sch : Schema.Aux[ T, S ],
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
){
   def primitive : FieldDescriptionBuilderWithSchemaWithoutName[ T, Unit ] = {
       FieldDescriptionBuilderWithSchemaWithoutName[ T, Unit ](
           Primitive[ T ](),
           desc,
           vs,
       )
   }

   def fromSchema[ S ]( implicit schema : Schema.Aux[ T, S ] ) : FieldDescriptionBuilderWithSchemaWithoutName[ T, S ] = {
       FieldDescriptionBuilderWithSchemaWithoutName[ T, S ](
           schema,
           desc,
           vs,
       )
   }

   def buildSchema[ Rt, S ]( builder : SchemaBuilder[ T ] => Schema.Aux[ T, S ] ) : FieldDescriptionBuilderWithSchemaWithoutName[ T, S ] = {
       fromSchema( builder( SchemaBuilder[ T ] ) )
   }

   def fieldName[ N <: FieldName ]( name : N ) : BuildableFieldDescriptionBuilder[ T, N, S ] =
        BuildableFieldDescriptionBuilder[ T, N, S ]( sch, name, desc, vs )
   def description( description : String ) : FieldDescriptionBuilderWithSchemaWithoutName[ T, S ] = copy( desc = Some( description ) )
   def validate( validators : Validator[ T ]* ) : FieldDescriptionBuilderWithSchemaWithoutName[ T, S ] = copy( vs = validators.toSet )
}

case class BuildableFieldDescriptionBuilder[ T, N <: FieldName, S ](
   private val sch : Schema.Aux[ T, S ],
   private val fn : N,
   private val desc : Option[ String ] = None,
   private val vs : Set[ Validator[ T ] ],
) {
   def fieldName[ NewN <: FieldName ]( name : NewN ) : BuildableFieldDescriptionBuilder[ T, NewN, S ] =
       copy[ T, NewN, S ]( fn = name )
   def description( description : String ) : BuildableFieldDescriptionBuilder[ T, N, S ] =
       copy( desc = Some( description ) )
   def validate( validators : Validator[ T ]* ) : BuildableFieldDescriptionBuilder[ T, N, S ] =
       copy( vs = validators.toSet )

    def primitive : BuildableFieldDescriptionBuilder[ T, N, Unit ] = {
        BuildableFieldDescriptionBuilder[ T, N, Unit ](
            Primitive[ T ](),
            fn,
            desc,
            vs,
        )
    }

    def fromSchema[ NewS ]( implicit schema : Schema.Aux[ T, NewS ] ) : BuildableFieldDescriptionBuilder[ T, N, NewS ] = {
        BuildableFieldDescriptionBuilder[ T, N, NewS ](
            schema,
            fn,
            desc,
            vs,
        )
    }

    def buildSchema[ Rt, NewS ]( builder : SchemaBuilder[ T ] => Schema.Aux[ T, NewS ] ) : BuildableFieldDescriptionBuilder[ T, N, NewS ] = {
        fromSchema[ NewS ]( builder( SchemaBuilder[ T ] ) )
    }

    def rebuildSchema(
        using
        srb : SchemaRebuilder[ T, S ],
    ) : BuildableFieldSchemaRebuilder[ T, N, srb.Builder ] = {
        BuildableFieldSchemaRebuilder[ T, N, srb.Builder ](
            srb.rebuild( sch ),
            fn,
            desc,
            vs,
        )
    }

   def build : FieldDescription.Aux[ T, N, S ] = {
       FieldDescriptionCase[ T, N, S ]( fn, sch, desc, vs )
   }
}

object FieldDescriptionBuilder {
   def apply[ T ] : FieldDescriptionBuilderWithoutSchemaOrName[ T ] = FieldDescriptionBuilderWithoutSchemaOrName[ T ]()

   def from[ T ]( fieldDescription: FieldDescription[ T ] ) : BuildableFieldDescriptionBuilder[ T, fieldDescription.Name, fieldDescription.Shape ] = {
       BuildableFieldDescriptionBuilder[ T, fieldDescription.Name, fieldDescription.Shape ](
           fieldDescription.schema,
           fieldDescription.fieldName,
           fieldDescription.description,
           fieldDescription.validators,
       )
   }
}

case class BuildableFieldSchemaRebuilder[ T, N <: FieldName, Builder ](
    private val builder : Builder,
    private val fn : N,
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ],
) {
    def apply[ S ](
        build : Builder => Schema.Aux[ T, S ],
    ) : BuildableFieldDescriptionBuilder[ T, N, S ] = {
        BuildableFieldDescriptionBuilder[ T, N, S ](
            build( builder ),
            fn,
            desc,
            vs,
        )
    }
}
