package org.hungerford.generic.schema.example.api

import org.hungerford.generic.schema.example.api.DataModel.Request
import sttp.tapir.*
import sttp.tapir.json.circe.*

object RestApi {
    val transactionEndpoint = endpoint
      .post
      .in( "transactions" )
      .in( jsonBody[ Request ] )
      .out()
}
