package org.hungerford.generic.schema.example.api

import org.hungerford.generic.schema.example.api.DataModel.{Response, Request, Date}
import generic.schema.exports.*
import sttp.tapir.Schema as TapirSchema

object DataSchema {

    val requestSchema = {
        import generic.schema.defaults.given

        val dateSch = Schema.derived[ Date ]
          .modifyComponent( "year" )(
              _.withValidators( Validator.min( 2000 ), Validator.max( 3000 ) )
          )
          .modifyComponent( "month" )(
              _.withValidators( Validator.min( 1 ), Validator.max( 12 ) )
          )
        import dateSch.givenSchema

        Schema.derived[ Request ]
    }

    val responseSchema = {
        import generic.schema.defaults.given
        Schema.derived[ Response ]
    }

    given TapirSchema[ Request ] = {
        import generic.schema.tapir.given
        requestSchema.as[TapirSchema]
    }

    given TapirSchema[ Response ] = {
        import generic.schema.tapir.given
        responseSchema.as[TapirSchema]
    }

}
