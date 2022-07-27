package org.hungerford.generic.schema.circe

import io.circe.Decoder.Result
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.translation.{TypeCache, RecursiveSchemaTranslator, SchemaTranslator}
import org.hungerford.generic.schema.coproduct.subtype.TypeName
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}
import org.hungerford.generic.schema.Schema.Aux

trait CirceSingletonSchemaTranslation {

    given singletonDecoderTrans[ T <: Singleton, N <: TypeName, Cache <: TypeCache ](
        using
        strCodec : Decoder[ String ],
    ) : RecursiveSchemaTranslator[ T, SingletonShape[ T, N ], Cache, Decoder ] with {
        override def translate(
            schema: Aux[ T, SingletonShape[ T, N ] ],
            cache: Cache,
        ): Decoder[ T ] = new Decoder[ T ] {
            override def apply( c: HCursor ): Result[ T ] = c.as[ String ] match {
                case Left( failure ) => Left( failure )
                case Right( nm ) if nm == schema.shape.name =>
                    Right( schema.shape.value )
                case _ =>
                    Left( DecodingFailure.apply( "string value does not match singleton label", Nil ) )
            }
        }
    }

    given singletonEncoderTrans[ T <: Singleton, N <: TypeName, Cache <: TypeCache ](
        using
        strCodec : Encoder[ String ],
    ) : RecursiveSchemaTranslator[ T, SingletonShape[ T, N ], Cache, Encoder ] with {
        override def translate(
            schema: Aux[ T, SingletonShape[ T, N ] ],
            cache: Cache,
        ): Encoder[ T ] = new Encoder[ T ] {
                override def apply( a: T ): Json = strCodec( schema.shape.name )
            }
    }

}

object CirceSingletonSchemaTranslation
  extends CirceSingletonSchemaTranslation
