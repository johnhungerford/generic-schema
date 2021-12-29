package org.hungerford.generic.schema.circe

import io.circe.{Codec, Encoder, Decoder}

trait CirceSchemaTranslation
  extends CirceProductSchemaTranslation
    with CirceCoproductSchemaTranslation {

    given [ T ](
      using
        enc : Encoder[ T ],
        dec : Decoder[ T ],
    ) : Codec[ T ] = Codec.from( dec, enc )

}

object CirceSchemaTranslation
  extends CirceSchemaTranslation
