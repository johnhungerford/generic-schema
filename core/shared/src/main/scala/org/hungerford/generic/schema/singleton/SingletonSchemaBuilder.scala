package org.hungerford.generic.schema.singleton

import org.hungerford.generic.schema.{ComplexSchema, Primitive, PrimitiveSchemaBuilder, Schema}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.coproduct.subtype.TypeName
import org.hungerford.generic.schema.types.Sub

case class SingletonSchemaBuilder[ T <: Singleton, N ](
    private[ schema ] val nm : Option[ String ] = None,
    private[ schema ] val desc : Option[ String ] = None,
    private[ schema ] val dep : Boolean = false,
    private[ schema ] val idnm : N,
) {

    def name( name : String ) : SingletonSchemaBuilder[ T, N ] = copy( nm = Some( name ) )

    def description( description : String ) : SingletonSchemaBuilder[ T, N ] = copy( desc = Some( description ) )

    def deprecated : SingletonSchemaBuilder[ T, N ] = copy( dep = true )

    def identifier[ NewN <: TypeName ]( id : NewN ) : SingletonSchemaBuilder[ T, NewN ] = copy( idnm = id )

    def build[ NV <: TypeName ](
        using
        ev : Sub.Aux[ N, TypeName, NV ],
        vo : ValueOf[ T ],
    ) : Schema.Aux[ T, SingletonShape[ T, NV ] ] = ComplexSchema(
        shape = SingletonShape[ T, NV ](
            ev( idnm ),
            vo.value,
        ),
        name = nm,
        genericDescription = desc,
        deprecated = dep,
    )
}

