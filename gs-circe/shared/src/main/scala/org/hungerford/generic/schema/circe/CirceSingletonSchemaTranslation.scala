package org.hungerford.generic.schema.circe

import io.circe.Decoder.Result
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator}
import org.hungerford.generic.schema.coproduct.subtype.TypeName
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}
import org.hungerford.generic.schema.Schema.Aux

trait CirceSingletonSchemaTranslation {

    given singletonDecoderTrans[ T <: Singleton, N <: TypeName, Trans <: Tuple ](
        using
        strCodec : Decoder[ String ],
    ) : RecursiveSchemaTranslator[ T, SingletonShape[ T, N ], Trans, Decoder ] with {
        override def translate(
            schema: Aux[ T, SingletonShape[ T, N ] ],
            trans: Trans,
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

    given singletonEncoderTrans[ T <: Singleton, N <: TypeName, Trans <: Tuple ](
        using
        strCodec : Encoder[ String ],
    ) : RecursiveSchemaTranslator[ T, SingletonShape[ T, N ], Trans, Encoder ] with {
        override def translate(
            schema: Aux[ T, SingletonShape[ T, N ] ],
            trans: Trans,
        ): Encoder[ T ] = new Encoder[ T ] {
                override def apply( a: T ): Json = strCodec( schema.shape.name )
            }
    }

}

object CirceSingletonSchemaTranslation
  extends CirceSingletonSchemaTranslation
