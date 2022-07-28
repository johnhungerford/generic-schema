package org.hungerford.generic.schema.example.api

import org.hungerford.generic.schema.example.api.DataModel.{Request, Response}

import scala.util.Try
import sttp.tapir.*
import sttp.tapir.EndpointOutput.StatusCode
import sttp.tapir.json.circe.*
import sttp.apispec.openapi.circe.yaml.*
import sttp.apispec.openapi.OpenAPI
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import zio.Task

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


object RestApi extends Serialization {
    import DataSchema.given
    import Serialization.given

    val transactionEndpoint = endpoint
      .post
      .in( "transactions" )
      .in( jsonBody[ Request ] )
      .out( jsonBody[ Response ] )

    val swaggerApiName : String = "Transactions"
    val swaggerApiVersion : String = "1.0"

    def swaggerServerEndpoints[F[_]]: List[ ServerEndpoint[ Any, F ] ] =
        SwaggerInterpreter()
          .fromEndpoints[F](
              List(
                  transactionEndpoint,
              ),
              swaggerApiName,
              swaggerApiVersion,
          )

    lazy val docs : OpenAPI = OpenAPIDocsInterpreter().toOpenAPI( transactionEndpoint, swaggerApiVersion, swaggerApiVersion )

    lazy val docsString : String = docs.toYaml


}
