package org.hungerford.generic.schema.circe

import io.circe.Decoder.Result
import io.circe.{Codec, Decoder, Encoder, HCursor, Json}
import io.circe.DecodingFailure
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.{Schema, SchemaProvider}
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, TypeName}
import org.hungerford.generic.schema.coproduct.translation.BiMapCoproductTranslation
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator}
import org.hungerford.generic.schema.product.field.FieldName

import scala.util.{Failure, Success, Try}

trait CirceCoproductSchemaTranslation
  extends BiMapCoproductTranslation[ Codec, Decoder, Encoder, Json, Json ] {

    def buildCoproductSchema[ T ]( enc : Encoder[ T ], dec : Decoder[ T ] ) : Codec[ T ] =
        Codec.from( dec, enc )

    def buildCoproductDecoder[ T ](
        decode: Json => Option[ T ]
    ): Decoder[ T ] = Decoder.instance( cursor => decode( cursor.value ) match {
        case None => Left( DecodingFailure( "failed", Nil ) )
        case Some( v ) => Right( v )
    } )

    def buildCoproductEncoder[ T ]( decode: T => Json ): Encoder[ T ] =
        Encoder.instance( decode )

    given [ ST, N <: TypeName ] : SubtypeReader[ ST, N ] with {
        def read( from : Json, subtype : Subtype.Tpe[ ST ] & Subtype.Named[ N ], schema : Decoder[ ST ] ) : Option[ ST ] =
            schema( from.hcursor ).toOption
    }

    given [ DV ] : DiscrReader[ DV ] with {
        def read( from : Json, name : String, schema : Decoder[ DV ] ) : Option[ DV ] =
            schema( from.hcursor ).toOption
    }

    given [ T, ST, N <: TypeName ] : SubtypeWriter[ ST, N ] with {
        def write( value : ST, subtype : Subtype.Tpe[ ST ] & Subtype.Named[ N ], schema : Encoder[ ST ] ) : Json =
            (schema : Encoder[ ST ])( value )
    }

}
