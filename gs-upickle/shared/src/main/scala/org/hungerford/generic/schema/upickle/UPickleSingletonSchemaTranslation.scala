package org.hungerford.generic.schema.upickle

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.translation.{TypeCache, RecursiveSchemaTranslator, SchemaTranslator}
import org.hungerford.generic.schema.coproduct.subtype.TypeName
import org.hungerford.generic.schema.Schema
import ujson.Value
import upickle.default.*

trait UPickleSingletonSchemaTranslation {

    given singletonDecoder[ T <: Singleton, N <: TypeName, Cache <: TypeCache ] : RecursiveSchemaTranslator[ T, SingletonShape[ T, N ], Cache, Reader ] with {
        override def translate(
            schema: Schema.Aux[ T, SingletonShape[ T, N ] ], cache : Cache,
        ): Reader[ T ] = {
            reader[ String ].map[ T ] { ( v : String ) =>
                if ( v == schema.shape.name )
                    schema.shape.value
                else throw new Exception( s"$v does not match subtype identifier: ${schema.shape.name}" )
            }
        }
    }

    given singletonEncoder[ T <: Singleton, N <: TypeName, Cache <: TypeCache ] : RecursiveSchemaTranslator[ T, SingletonShape[ T, N ], Cache, Writer ] with {
        override def translate(
            schema: Schema.Aux[ T, SingletonShape[ T, N ] ], cache : Cache,
        ): Writer[ T ] = {
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
