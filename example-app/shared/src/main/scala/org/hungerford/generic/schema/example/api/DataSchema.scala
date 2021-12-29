package org.hungerford.generic.schema.example.api

import org.hungerford.generic.schema.{Default, Schema}
import org.hungerford.generic.schema.example.api.DataModel.{Date, Request}
import org.hungerford.generic.schema.tapir.TapirSchemaTranslation
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.translation.SchemaTranslator
import sttp.tapir.Schema as TapirSchema

object DataSchema {
    
    val requestSchema = Default.usingDsl( dsl => {
        import dsl.{*, given}

        val dateSch = Schema.derived[ Date ]
          .modifyComponent( "year" )(
              _.withValidators( Validator.min( 2000 ), Validator.max( 3000 ) )
              )
          .modifyComponent( "month" )(
              _.withValidators( Validator.min( 1 ), Validator.max( 12 ) )
              )
        import dateSch.givenSchema

        Schema.derived[ Request ]
    } )

    given TapirSchema[ Request ] = {
        import TapirSchemaTranslation.{*, given}

        SchemaTranslator.translate( requestSchema )
    }

}
