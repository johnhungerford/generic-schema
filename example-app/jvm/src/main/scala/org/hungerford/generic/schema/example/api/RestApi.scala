package org.hungerford.generic.schema.example.api

import org.hungerford.generic.schema.example.api.DataModel.Request
import sttp.tapir.*
import sttp.tapir.EndpointOutput.StatusCode
import sttp.tapir.json.circe.*

import sttp.apispec.openapi.circe.yaml._
import sttp.apispec.openapi.OpenAPI
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter


object RestApi extends Serialization {
    import DataSchema.given

    val transactionEndpoint = endpoint
      .post
      .in( "transactions" )
      .in( jsonBody[ Request ] )

    lazy val docs : OpenAPI = OpenAPIDocsInterpreter().toOpenAPI( transactionEndpoint, "Transactions", "1.0" )

    lazy val docsString : String = docs.toYaml
}
