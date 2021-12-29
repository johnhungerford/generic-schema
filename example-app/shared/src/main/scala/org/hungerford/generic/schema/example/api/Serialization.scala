package org.hungerford.generic.schema.example.api

import org.hungerford.generic.schema.example.api.DataModel.Request
import org.hungerford.generic.schema.translation.SchemaTranslator
import io.circe.Codec
import org.hungerford.generic.schema.circe.CirceSchemaTranslation

trait Serialization {

    import CirceSchemaTranslation.given

    given requestCodec : Codec[ Request ] = SchemaTranslator.translate( DataSchema.requestSchema )

}

object Serialization extends Serialization
