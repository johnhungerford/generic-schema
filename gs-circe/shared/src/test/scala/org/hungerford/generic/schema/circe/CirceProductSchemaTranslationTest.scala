package org.hungerford.generic.schema.circe

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.translation.{ProductJsonTranslationTest, SchemaTranslator}
import io.circe.{Decoder as CirceDecoder, Encoder as CirceEncoder, *, given}
import io.circe.syntax.*
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.translation.{Decoder, Encoder}

import scala.util.NotGiven

import CirceSchemaTranslation.given

class CirceProductSchemaTranslationTest
  extends ProductJsonTranslationTest[ Codec ] {

    def writeJson[ T ]( value : T, schm : Codec[ T ] ) : String = {
        schm( value ).noSpaces.toString
    }

}
