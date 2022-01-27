package org.hungerford.generic.schema.product.field

import org.hungerford.generic.schema.types.Sub
import org.hungerford.generic.schema.product.field.{Field, FieldCase}
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{ComplexSchema, Primitive, Schema, SchemaRebuilder}
import org.hungerford.generic.schema.coproduct.subtype.TypeName


case class FieldBuilder[ T, F, N, Sch, S, E ](
    private[ schema ] val nm: N,
    private[ schema ] val extr: E,
    private[ schema ] val sch : Sch,
    private[ schema ] val desc : Option[ String ] = None,
    private[ schema ] val vs : Set[ Validator[ F ] ] = Set.empty[ Validator[ F ] ],
    private[ schema ] val exs : Seq[ F ] = Nil,
    private[ schema ] val df : Option[ F ] = None,
    private[ schema ] val dep : Boolean = false,
) {
    private type CurrentFB = FieldBuilder[ T, F, N, Sch, S, E ]

    def name[ NewN <: FieldName ]( newName : NewN ) : FieldBuilder[ T, F, NewN, Sch, S, E ] =
        copy( nm = newName )
    def extractor( extract : T => F ) : FieldBuilder[ T, F, N, Sch, S, T => F ] = copy( extr = extract )
    def fromSchema[ NewS ]( implicit schema: Schema.Aux[ F, NewS ] ) : FieldBuilder[ T, F, N, Schema.Aux[ F, NewS ], NewS, E ] =
        copy( sch = schema )
    def rebuildSchema(
        using
        ev : Sch <:< Schema.Aux[ F, S ],
        rb : SchemaRebuilder[ F, S ],
    ) : FieldSchemaRebuilder[ T, F, N, S, E, rb.Builder ] = FieldSchemaRebuilder[ T, F, N, S, E, rb.Builder ]( this.asInstanceOf[ FieldBuilder[ T, F, N, Schema.Aux[ F, S ], S, E ] ] )
    def primitive : FieldBuilder[ T, F, N, Primitive[ F ], Unit, E ] = copy( sch = Primitive[ F ]() )
    def singleton[ SN <: TypeName, SV <: Singleton & F ]( value : SV, identifier : SN ) : FieldBuilder[ T, F, N, Schema.Aux[ F, SingletonShape[ SV, SN ] ], SingletonShape[ SV, SN ], E ] =
        fromSchema[ SingletonShape[ SV, SN ] ]( ComplexSchema( SingletonShape( identifier, value ) ) )
    def description( newDescription : String ): CurrentFB = copy( desc = Some( newDescription ) )
    def validation( validators : Validator[ F ]* ): CurrentFB = copy( vs = validators.toSet )
    def examples( ex : F* ) : CurrentFB = copy( exs = exs.toSeq )
    def default( defaultValue : F ) : CurrentFB = copy( df = Some( defaultValue ) )
    def deprecated : CurrentFB = copy( dep = true )
}

object FieldBuilder {
    type Empty[ T, F ] = FieldBuilder[ T, F, Unit, Unit, Nothing, Unit ]
    
    extension [ T, F, N <: FieldName, Sch <: Schema.Aux[ F, S ], S, E ]( fieldBuilder : FieldBuilder[ T, F, N, Sch, S, T => F ] )
        def build : Field.Aux[ T, F, N, S ] =
            FieldCase[ T, F, N, S ](
                fieldBuilder.nm,
                fieldBuilder.extr,
                fieldBuilder.sch,
                fieldBuilder.desc,
                fieldBuilder.vs,
                fieldBuilder.df,
                fieldBuilder.exs,
                fieldBuilder.dep,
            )

    def apply[ T, F ] : Empty[ T, F ] =
        FieldBuilder[ T, F, Unit, Unit, Nothing, Unit ]((),(),(),None,Set.empty[Validator[F]],Nil,None,false)

    def from[ T, F, N <: FieldName, S ]( field : Field.Aux[ T, F, N, S ] ) : FieldBuilder[ T, F, N, Schema.Aux[ F, S ], S, T => F ] =
        FieldBuilder[ T, F, N, Schema.Aux[ F, S ], S, T => F ](
            field.fieldName,
            field.extractor,
            field.schema,
            field.description,
            field.validators,
            field.examples,
            field.default,
            field.deprecated,
        )
}

case class FieldSchemaRebuilder[ T, F, N, S, E, Builder ](
    fieldBuilder: FieldBuilder[ T, F, N, Schema.Aux[ F, S ], S, E ],
)(
    using
    srb : SchemaRebuilder.Aux[ F, S, Builder ],
) {
    def apply[ NewS ](
        build : Builder => Schema.Aux[ F, NewS ],
    ) : FieldBuilder[ T, F, N, Schema.Aux[ F, NewS ], NewS, E ] =
        fieldBuilder.copy( sch = build( srb.rebuild( fieldBuilder.sch ) ) )
}
