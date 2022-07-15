package org.hungerford.generic.schema.circe

import io.circe.Decoder.{Result, currencyDecoder}
import org.hungerford.generic.schema.product.field.{Field, FieldName, LazyField}
import org.hungerford.generic.schema.product.translation.BiMapProductTranslation
import io.circe.{ACursor, Codec, Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.ProductConstructor
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator, TransRetriever}

import scala.compiletime.{erasedValue, error, summonFrom, summonInline}
import scala.collection.mutable
import scala.util.Try

trait CirceProductSchemaTranslation
  extends BiMapProductTranslation[ Codec, Decoder, Encoder, Json, Json ] {

    def buildProductSchema[ T ](
        enc: Encoder[ T ], dec: Decoder[ T ]
    ): Codec[ T ] = Codec.from( dec, enc )

    def mergeFields[ T ](
        fieldMap: List[ (String, Json) ]
    ): Json = Json.obj( fieldMap* )

    def getFieldNames(
        from: Json
    ): Set[ String ] = from.hcursor.keys.toSet.flatMap( _.toSet )

    def writeField[ T ]( value: T, encoder: Encoder[ T ] ): Json = encoder( value )

    def getField[ AF ](
        from: Json, fieldName: String, decoder: Decoder[ AF ]
    ): Option[ AF ] = from.hcursor.downField( fieldName ).as[ AF ]( decoder ).toOption

    def buildProductDecoder[ T ](
        decode: Json => Option[ T ]
    ): Decoder[ T ] = Decoder.instance( cursor => decode( cursor.value ) match {
        case None => Left( DecodingFailure( "failed", Nil ) )
        case Some( v ) => Right( v )
    } )

    def buildProductEncoder[ T ]( decode: T => Json ): Encoder[ T ] =
        Encoder.instance( decode )

}
