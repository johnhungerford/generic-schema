package org.hungerford.generic.schema.circe

import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.translation.{TypeCache, RecursiveSchemaTranslator, SchemaTranslator}

import scala.compiletime.{erasedValue, error}

trait CirceSchemaTranslation
  extends CirceProductSchemaTranslation
    with CirceCoproductSchemaTranslation
    with CirceSingletonSchemaTranslation {

    given [ T, S, Cache <: TypeCache ](
        using
        encTr: RecursiveSchemaTranslator[ T, S, TypeCache.Empty, Encoder ],
        decTr: RecursiveSchemaTranslator[ T, S, TypeCache.Empty, Decoder ],
    ): RecursiveSchemaTranslator[ T, S, Cache, Codec ] with {
        override def translate(
            schema: Schema.Aux[ T, S ],
            trans: Cache,
        ): Codec[ T ] = {
            val encoder = encTr.translate( schema, TypeCache.Empty )
            val decoder = decTr.translate( schema, TypeCache.Empty )
            Codec.from( decoder, encoder )
        }
    }
}

object CirceSchemaTranslation
  extends CirceSchemaTranslation
