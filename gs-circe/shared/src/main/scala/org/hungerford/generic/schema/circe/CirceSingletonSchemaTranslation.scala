package org.hungerford.generic.schema.circe

import io.circe.Decoder.Result
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.coproduct.subtype.TypeName
import io.circe.{Codec, Decoder, Encoder, HCursor, Json, DecodingFailure}
import org.hungerford.generic.schema.Schema.Aux

trait CirceSingletonSchemaTranslation {

    given [ T <: Singleton, N <: TypeName ](
        using
        strCodec : Codec[ String ],
    ) : SchemaTranslator[ T, SingletonShape[ T, N ], Codec ] with {
        override def translate(
            schema: Aux[ T, SingletonShape[ T, N ] ]
        ): Codec[ T ] = {
            val encoder = new Encoder[ T ] {
                override def apply( a: T ): Json = strCodec( schema.shape.name )
            }

            val decoder = new Decoder[ T ] {
                override def apply( c: HCursor ): Result[ T ] = c.as[ String ] match {
                    case Left( failure ) => Left( failure )
                    case Right( nm ) if nm == schema.shape.name =>
                        Right( schema.shape.value )
                    case _ =>
                        Left( DecodingFailure.apply( "string value does not match singleton label", Nil ) )
                }
            }

            Codec.from( decoder, encoder )
        }
    }

}

object CirceSingletonSchemaTranslation
  extends CirceSingletonSchemaTranslation
