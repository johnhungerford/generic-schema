package org.hungerford.generic.schema.example.api

import org.hungerford.generic.schema.example.api.DataModel.Request
import org.hungerford.generic.schema.translation.SchemaTranslator
import io.circe.Codec
import org.hungerford.generic.schema.circe.CirceSchemaTranslation

trait Serialization {

    import org.hungerford.generic.schema.Default.dsl.{*, given}

    import CirceSchemaTranslation.given

    import DataSchema.requestSchema.givenSchema

    given requestCodec : Codec[ Request ] = DataSchema.requestSchema.as[ Codec ]

}

object Serialization extends Serialization
