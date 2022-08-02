package org.hungerford.generic.schema.example.api

import org.hungerford.generic.schema.example.api.DataModel.{Request, Response}
import org.hungerford.generic.schema.translation.SchemaTranslator
import io.circe.Codec
import org.hungerford.generic.schema.circe.CirceSchemaTranslation
import io.circe.parser._

trait Serialization {

    import generic.schema.exports.{*, given}

    import CirceSchemaTranslation.given

    import DataSchema.requestSchema.givenSchema

    given requestCodec : Codec[ Request ] = DataSchema.requestSchema.as[ Codec ]

    given responseCodec : Codec[ Response ] = DataSchema.responseSchema.as[ Codec ]

}

object Serialization extends Serialization
