package org.hungerford.generic.schema.example.api

import org.hungerford.generic.schema.example.api.DataModel.{Request, Response}
import org.hungerford.generic.schema.translation.SchemaTranslator
import io.circe.Codec
import org.hungerford.generic.schema.circe.CirceSchemaTranslation
import io.circe.parser._

trait Serialization {

    import org.hungerford.generic.schema.Default.dsl.{*, given}

    import CirceSchemaTranslation.given

    import DataSchema.requestSchema.givenSchema

    given requestCodec : Codec[ Request ] = DataSchema.requestSchema.as[ Codec ]

    given responseCodec : Codec[ Response ] = DataSchema.responseSchema.as[ Codec ]

    val requestExample = requestCodec.decodeJson( parse("""{
                                                          |	"name": "John Hungerford",
                                                          |	"id": "my-id",
                                                          |	"amount": 23423,
                                                          |	"payment": {
                                                          |		"number": 123,
                                                          |		"expDat": {
                                                          |			"req": {
                                                          |	"name": "John Hungerford",
                                                          |	"id": "my-id",
                                                          |	"amount": 23423,
                                                          |	"payment": {
                                                          |		"number": 123,
                                                          |		"expDat": {
                                                          |			"year": 2020,
                                                          |			"month": 6
                                                          |		}
                                                          |	}
                                                          |}
                                                          |	}
                                                          |}
                                                          |}""".stripMargin).toTry.get )

}

object Serialization extends Serialization
