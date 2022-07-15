package org.hungerford.generic.schema.circe

import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator}

import scala.compiletime.{erasedValue, error}

trait CirceSchemaTranslation
  extends CirceProductSchemaTranslation
    with CirceCoproductSchemaTranslation
    with CirceSingletonSchemaTranslation {

    given [ T ](
        using
        enc : Encoder[ T ],
        dec : Decoder[ T ],
    ) : Codec[ T ] = Codec.from( dec, enc )

    given [ T, S, Trans <: Tuple ](
        using
        encTr: RecursiveSchemaTranslator[ T, S, EmptyTuple, Encoder ],
        decTr: RecursiveSchemaTranslator[ T, S, EmptyTuple, Decoder ],
    ): RecursiveSchemaTranslator[ T, S, Trans, Codec ] with {
        override def translate(
            schema: Aux[ T, S ],
            trans: Trans,
        ): Codec[ T ] = {
            val encoder = encTr.translate( schema, EmptyTuple )
            val decoder = decTr.translate( schema, EmptyTuple )
            Codec.from( decoder, encoder )
        }
    }
}

object CirceSchemaTranslation
  extends CirceSchemaTranslation
