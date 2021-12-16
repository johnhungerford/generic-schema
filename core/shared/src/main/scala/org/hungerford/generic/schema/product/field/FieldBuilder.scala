package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{Primitive, Schema, SchemaRebuilder}


case class FieldBuilderWithoutSchemaOrName[ T ](
   private val desc : Option[ String ] = None,
   private val vs : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private val exs : Seq[ T ] = Nil,
   private val df : Option[ T ] = None,
   private val dep : Boolean = false,
){
   def primitive : FieldBuilderWithSchemaWithoutName[ T, Unit ] = {
       FieldBuilderWithSchemaWithoutName[ T, Unit ](
           Primitive[ T ](),
           desc,
           vs,
           exs,
           df,
           dep,
       )
   }

   def fromSchema[ S ]( implicit schema : Schema.Aux[ T, S ] ) : FieldBuilderWithSchemaWithoutName[ T, S ] = {
       FieldBuilderWithSchemaWithoutName[ T, S ](
           schema,
           desc,
           vs,
           exs,
           df,
           dep,
       )
   }

   def fieldName[ N <: FieldName ]( name : N ) : FieldBuilderWithoutSchemaWithName[ T, N ] =
       FieldBuilderWithoutSchemaWithName[ T, N ]( name, desc, vs )
   def description( description : String ) : FieldBuilderWithoutSchemaOrName[ T ] = copy( desc = Some( description ) )
   def validate( validators : Validator[ T ]* ) : FieldBuilderWithoutSchemaOrName[ T ] =  copy( vs = vs ++ validators.toSet )
   def examples( examples: T* ): FieldBuilderWithoutSchemaOrName[ T ] =
     copy( exs = exs ++ examples )
}

case class FieldBuilderWithoutSchemaWithName[ T, N <: FieldName ](
    private val fn : N,
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private val exs : Seq[ T ] = Nil,
    private val df : Option[ T ] = None,
    private val dep : Boolean = false,
){
   def primitive : BuildableFieldBuilder[ T, N, Unit ] = {
       BuildableFieldBuilder[ T, N, Unit ](
           Primitive[ T ](),
           fn,
           desc,
           vs,
           exs,
           df,
           dep,
       )
   }

   def fromSchema[ S ]( implicit schema : Schema.Aux[ T, S ] ) : BuildableFieldBuilder[ T, N, S ] = {
       BuildableFieldBuilder[ T, N, S ](
           schema,
           fn,
           desc,
           vs,
           exs,
           df,
           dep,
       )
   }

   def fieldName[ NewN <: FieldName ]( name : NewN ) : FieldBuilderWithoutSchemaWithName[ T, NewN ] = copy[ T, NewN ]( fn = name )
   def description( description : String ) : FieldBuilderWithoutSchemaWithName[ T, N ] = copy( desc = Some( description ) )
   def validate( validators : Validator[ T ]* ) : FieldBuilderWithoutSchemaWithName[ T, N ] =  copy( vs = vs ++ validators.toSet )
   def examples(examples: T*): FieldBuilderWithoutSchemaWithName[ T, N ] =
     copy(exs = examples)
   def default( defaultValue : T ) : FieldBuilderWithoutSchemaWithName[ T, N ] = copy( df = Some( defaultValue ) )
   def deprecate : FieldBuilderWithoutSchemaWithName[ T, N ] = copy( dep = true )
}

case class FieldBuilderWithSchemaWithoutName[ T, S ](
    private val sch : Schema.Aux[ T, S ],
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
    private val exs : Seq[ T ] = Nil,
    private val df : Option[ T ] = None,
    private val dep : Boolean = false,
){
   def primitive : FieldBuilderWithSchemaWithoutName[ T, Unit ] = {
       FieldBuilderWithSchemaWithoutName[ T, Unit ](
           Primitive[ T ](),
           desc,
           vs,
           exs,
           df,
           dep,
       )
   }

   def fromSchema[ S ]( implicit schema : Schema.Aux[ T, S ] ) : FieldBuilderWithSchemaWithoutName[ T, S ] = {
       FieldBuilderWithSchemaWithoutName[ T, S ](
           schema,
           desc,
           vs,
           exs,
           df,
           dep,
       )
   }

    def rebuildSchema(
        using
        srb : SchemaRebuilder[ T, S ],
    ) : NamelessFieldSchemaRebuilder[ T, srb.Builder ] = {
        NamelessFieldSchemaRebuilder[ T, srb.Builder ](
            srb.rebuild( sch ),
            desc,
            vs,
            exs,
            df,
            dep,
        )
    }

    def fieldName[ N <: FieldName ]( name : N ) : BuildableFieldBuilder[ T, N, S ] =
        BuildableFieldBuilder[ T, N, S ]( sch, name, desc, vs )

    def description( description : String ) : FieldBuilderWithSchemaWithoutName[ T, S ] = copy( desc = Some( description ) )
    def validate( validators : Validator[ T ]* ) : FieldBuilderWithSchemaWithoutName[ T, S ] =  copy( vs = vs ++ validators.toSet )
    def examples( examples : T* ) : FieldBuilderWithSchemaWithoutName[ T, S ] = copy( exs = exs ++ examples )
    def default( defaultValue : T ) : FieldBuilderWithSchemaWithoutName[ T, S ] = copy( df = Some( defaultValue ) )
    def deprecate : FieldBuilderWithSchemaWithoutName[ T, S ] = copy( dep = true )
}

case class BuildableFieldBuilder[ T, N <: FieldName, S ](
   private val sch : Schema.Aux[ T, S ],
   private val fn : N,
   private val desc : Option[ String ] = None,
   private val vs : Set[ Validator[ T ] ] = Set.empty[ Validator[ T ] ],
   private val exs : Seq[ T ] = Nil,
   private val df : Option[ T ] = None,
   private val dep : Boolean = false,
) {
   def fieldName[ NewN <: FieldName ]( name : NewN ) : BuildableFieldBuilder[ T, NewN, S ] =
     copy[ T, NewN, S ]( fn = name )
   def description( description : String ) : BuildableFieldBuilder[ T, N, S ] = copy( desc = Some( description ) )
   def validate( validators : Validator[ T ]* ) : BuildableFieldBuilder[ T, N, S ] =  copy( vs = vs ++ validators.toSet )
   def examples( examples : T* ) : BuildableFieldBuilder[ T, N, S ] = copy( exs = exs ++ examples )
   def default( defaultValue : T ) : BuildableFieldBuilder[ T, N, S ] = copy( df = Some( defaultValue ) )
   def deprecate : BuildableFieldBuilder[ T, N, S ] = copy( dep = true )

    def primitive : BuildableFieldBuilder[ T, N, Unit ] = {
        BuildableFieldBuilder[ T, N, Unit ](
            Primitive[ T ](),
            fn,
            desc,
            vs,
            exs,
            df,
            dep,
        )
    }

    def fromSchema[ NewS ]( implicit schema : Schema.Aux[ T, NewS ] ) : BuildableFieldBuilder[ T, N, NewS ] = {
        BuildableFieldBuilder[ T, N, NewS ](
            schema,
            fn,
            desc,
            vs,
            exs,
            df,
            dep,
        )
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
            exs,
            df,
            dep,
        )
    }

   def build : Field.Aux[ T, N, S ] = {
       FieldCase[ T, N, S ]( fn, sch, desc, vs, df, exs, dep )
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
           fieldDescription.examples,
           fieldDescription.default,
           fieldDescription.deprecated,
       )
   }
}

case class NamelessFieldSchemaRebuilder[ T, Builder ](
    private val builder : Builder,
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ],
    private val exs : Seq[ T ] = Nil,
    private val df : Option[ T ] = None,
    private val dep : Boolean = false,
) {
    def apply[ S ](
        build : Builder => Schema.Aux[ T, S ],
    ) : FieldBuilderWithSchemaWithoutName[ T, S ] = {
        FieldBuilderWithSchemaWithoutName[ T, S ](
            build( builder ),
            desc,
            vs,
            exs,
            df,
            dep,
        )
    }
}

case class BuildableFieldSchemaRebuilder[ T, N <: FieldName, Builder ](
    private val builder : Builder,
    private val fn : N,
    private val desc : Option[ String ] = None,
    private val vs : Set[ Validator[ T ] ],
    private val exs : Seq[ T ] = Nil,
    private val df : Option[ T ] = None,
    private val dep : Boolean = false,
) {
    def apply[ S ](
        build : Builder => Schema.Aux[ T, S ],
    ) : BuildableFieldBuilder[ T, N, S ] = {
        BuildableFieldBuilder[ T, N, S ](
            build( builder ),
            fn,
            desc,
            vs,
            exs,
            df,
            dep,
        )
    }
}
