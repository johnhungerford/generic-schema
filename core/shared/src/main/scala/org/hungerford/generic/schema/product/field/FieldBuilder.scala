package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{Primitive, Schema, SchemaBuilder, SchemaRebuilder}


case class FieldBuilderWithoutSchemaOrName[ T ](
   private val desc : Option[ String ] = None,
   private val vs : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
){
   def primitive : FieldBuilderWithSchemaWithoutName[ T, Unit ] = {
       FieldBuilderWithSchemaWithoutName[ T, Unit ](
           Primitive[ T ](),
           desc,
           vs,
       )
   }

   def fromSchema[ S ]( implicit schema : Schema.Aux[ T, S ] ) : FieldBuilderWithSchemaWithoutName[ T, S ] = {
       FieldBuilderWithSchemaWithoutName[ T, S ](
           schema,
           desc,
           vs,
       )
   }

   def buildSchema[ Rt, S ]( builder : SchemaBuilder[ T ] => Schema.Aux[ T, S ] ) : FieldBuilderWithSchemaWithoutName[ T, S ] = {
       fromSchema( builder( SchemaBuilder[ T ] ) )
   }

   def fieldName[ N <: FieldName ]( name : N ) : FieldBuilderWithoutSchemaWithName[ T, N ] =
       FieldBuilderWithoutSchemaWithName[ T, N ]( name, desc, vs )
   def description( description : String ) : FieldBuilderWithoutSchemaOrName[ T ] = copy( desc = Some( description ) )
   def validate( validators : Validator[ T ]* ) : FieldBuilderWithoutSchemaOrName[ T ] = copy( vs = validators.toSet )
}

case class FieldBuilderWithoutSchemaWithName[ T, N <: FieldName ](
    private val fn : N,
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
){
   def primitive : BuildableFieldBuilder[ T, N, Unit ] = {
       BuildableFieldBuilder[ T, N, Unit ](
           Primitive[ T ](),
           fn,
           desc,
           vs,
       )
   }

   def fromSchema[ S ]( implicit schema : Schema.Aux[ T, S ] ) : BuildableFieldBuilder[ T, N, S ] = {
       BuildableFieldBuilder[ T, N, S ](
           schema,
           fn,
           desc,
           vs,
       )
   }

   def buildSchema[ Rt, S ]( builder : SchemaBuilder[ T ] => Schema.Aux[ T, S ] ) : BuildableFieldBuilder[ T, N, S ] = {
       fromSchema( builder( SchemaBuilder[ T ] ) )
   }

   def fieldName[ NewN <: FieldName ]( name : NewN ) : FieldBuilderWithoutSchemaWithName[ T, NewN ] = copy[ T, NewN ]( fn = name )
   def description( description : String ) : FieldBuilderWithoutSchemaWithName[ T, N ] = copy( desc = Some( description ) )
   def validate( validators : Validator[ T ]* ) : FieldBuilderWithoutSchemaWithName[ T, N ] = copy( vs = validators.toSet )
}

case class FieldBuilderWithSchemaWithoutName[ T, S ](
    private val sch : Schema.Aux[ T, S ],
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
){
   def primitive : FieldBuilderWithSchemaWithoutName[ T, Unit ] = {
       FieldBuilderWithSchemaWithoutName[ T, Unit ](
           Primitive[ T ](),
           desc,
           vs,
       )
   }

   def fromSchema[ S ]( implicit schema : Schema.Aux[ T, S ] ) : FieldBuilderWithSchemaWithoutName[ T, S ] = {
       FieldBuilderWithSchemaWithoutName[ T, S ](
           schema,
           desc,
           vs,
       )
   }

   def buildSchema[ Rt, S ]( builder : SchemaBuilder[ T ] => Schema.Aux[ T, S ] ) : FieldBuilderWithSchemaWithoutName[ T, S ] = {
       fromSchema( builder( SchemaBuilder[ T ] ) )
   }

    def rebuildSchema(
        using
        srb : SchemaRebuilder[ T, S ],
    ) : NamelessFieldSchemaRebuilder[ T, srb.Builder ] = {
        NamelessFieldSchemaRebuilder[ T, srb.Builder ](
            srb.rebuild( sch ),
            desc,
            vs,
        )
    }

    def fieldName[ N <: FieldName ]( name : N ) : BuildableFieldBuilder[ T, N, S ] =
        BuildableFieldBuilder[ T, N, S ]( sch, name, desc, vs )

    def description( description : String ) : FieldBuilderWithSchemaWithoutName[ T, S ] = copy( desc = Some( description ) )
    def validate( validators : Validator[ T ]* ) : FieldBuilderWithSchemaWithoutName[ T, S ] = copy( vs = validators.toSet )
}

case class BuildableFieldBuilder[ T, N <: FieldName, S ](
   private val sch : Schema.Aux[ T, S ],
   private val fn : N,
   private val desc : Option[ String ] = None,
   private val vs : Set[ Validator[ T ] ],
) {
   def fieldName[ NewN <: FieldName ]( name : NewN ) : BuildableFieldBuilder[ T, NewN, S ] =
       copy[ T, NewN, S ]( fn = name )
   def description( description : String ) : BuildableFieldBuilder[ T, N, S ] =
       copy( desc = Some( description ) )
   def validate( validators : Validator[ T ]* ) : BuildableFieldBuilder[ T, N, S ] =
       copy( vs = validators.toSet )

    def primitive : BuildableFieldBuilder[ T, N, Unit ] = {
        BuildableFieldBuilder[ T, N, Unit ](
            Primitive[ T ](),
            fn,
            desc,
            vs,
        )
    }

    def fromSchema[ NewS ]( implicit schema : Schema.Aux[ T, NewS ] ) : BuildableFieldBuilder[ T, N, NewS ] = {
        BuildableFieldBuilder[ T, N, NewS ](
            schema,
            fn,
            desc,
            vs,
        )
    }

    def buildSchema[ Rt, NewS ]( builder : SchemaBuilder[ T ] => Schema.Aux[ T, NewS ] ) : BuildableFieldBuilder[ T, N, NewS ] = {
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

   def build : Field.Aux[ T, N, S ] = {
       FieldCase[ T, N, S ]( fn, sch, desc, vs )
   }
}

object FieldBuilder {
   def apply[ T ] : FieldBuilderWithoutSchemaOrName[ T ] = FieldBuilderWithoutSchemaOrName[ T ]()

   def from[ T, N <: FieldName, S ]( fieldDescription: Field.Aux[ T, N, S ] ) : BuildableFieldBuilder[ T, N, S ] = {
       BuildableFieldBuilder[ T, N, S ](
           fieldDescription.schema,
           fieldDescription.fieldName,
           fieldDescription.description,
           fieldDescription.validators,
       )
   }
}

case class NamelessFieldSchemaRebuilder[ T, Builder ](
    private val builder : Builder,
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ],
) {
    def apply[ S ](
        build : Builder => Schema.Aux[ T, S ],
    ) : FieldBuilderWithSchemaWithoutName[ T, S ] = {
        FieldBuilderWithSchemaWithoutName[ T, S ](
            build( builder ),
            desc,
            vs,
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
    ) : BuildableFieldBuilder[ T, N, S ] = {
        BuildableFieldBuilder[ T, N, S ](
            build( builder ),
            fn,
            desc,
            vs,
        )
    }
}
