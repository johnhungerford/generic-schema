package org.hungerford.generic.schema.upickle

import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.coproduct.subtype.TypeName
import org.hungerford.generic.schema.Schema.Aux

import ujson.Value
import upickle.default.*

trait UPickleSingletonSchemaTranslation {

    given [ T <: Singleton, N <: TypeName ] : SchemaTranslator[ T, SingletonShape[ T, N ], ReadWriter ] with {
        override def translate(
            schema: Aux[ T, SingletonShape[ T, N ] ]
        ): ReadWriter[ T ] = {

            readwriter[ String ].bimap[ T ](
                ( t : T ) => schema.shape.name,
                ( v : String ) =>
                    if ( v == schema.shape.name )
                        schema.shape.value
                    else throw new Exception( s"$v does not match subtype identifier: ${schema.shape.name}" )
            )
        }
    }

}

object UPickleSingletonSchemaTranslation
  extends UPickleSingletonSchemaTranslation
