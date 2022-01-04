package org.hungerford.generic.schema.circe

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.translation.{ProductTranslationTest, SchemaTranslator}
import io.circe.{Decoder as CirceDecoder, Encoder as CirceEncoder, *, given}
import io.circe.syntax.*
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.translation.{Decoder, Encoder}

import scala.util.NotGiven

given [ T ](
    using
    enc : CirceEncoder[ T ],
    dec : CirceDecoder[ T ],
) : Codec[ T ] = Codec.from( dec, enc )

import CirceSchemaTranslation.given

class CirceProductSchemaTranslationTest
  extends ProductTranslationTest[ Codec ] {

    def writeJson[ T ]( value : T, schm : Codec[ T ] ) : String = {
        schm( value ).noSpaces.toString
    }

}
