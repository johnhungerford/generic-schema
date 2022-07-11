package org.hungerford.generic.schema.circe

import io.circe.{Codec, Decoder, Encoder}
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.translation.SchemaTranslator

trait CirceSchemaTranslation
  extends CirceProductSchemaTranslation
    with CirceCoproductSchemaTranslation
    with CirceSingletonSchemaTranslation {

    given [ T ](
        using
        enc : Encoder[ T ],
        dec : Decoder[ T ],
    ) : Codec[ T ] = Codec.from( dec, enc )

    given [ T, S ](
        using
        encTr: SchemaTranslator[ T, S, Encoder ],
        decTr: SchemaTranslator[ T, S, Decoder ],
    ): SchemaTranslator[ T, S, Codec ] with {
        override def translate(
            schema: Aux[ T, S ],
        ): Codec[ T ] = {
            val encoder = encTr.translate( schema )
            val decoder = decTr.translate( schema )
            Codec.from( decoder, encoder )
        }
    }
}

object CirceSchemaTranslation
  extends CirceSchemaTranslation
