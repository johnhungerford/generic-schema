package org.hungerford.generic.schema.circe

import io.circe.Decoder.{Result, currencyDecoder}
import org.hungerford.generic.schema.product.field.TranslatedFieldDescription
import org.hungerford.generic.schema.product.translation.BiMapProductTranslation
import io.circe.{Codec, Decoder, Encoder, HCursor, Json}

trait CirceProductSchemaTranslation
  extends BiMapProductTranslation[ Codec, Json, Map[ String, Json ] ] {

    /**
     * Construct a schema from the two parts of a bimap.
     *
     * @param to   T => MapVal : writer
     * @param from MapVal => T : reader
     * @tparam T type being read/written
     * @return type class instance for some reader/writer
     */
    protected def schemaFromBimap[ T ]( to : T => Json, from : Json => T ) : Codec[ T ] = {
        val encoder : Encoder[ T ] = ( a : T ) => to( a )

        val decoder : Decoder[ T ] = ( c: HCursor ) => {
            Right( from( c.value ) )
        }

        Codec.from( decoder, encoder )
    }

    /**
     * Initial empty value for the type being mapped to and from, that can be built
     * by adding field values. For instance, if the value type is Map[ String, T ],
     * initMapVal would be Map.empty[ String, T ]
     *
     * @return initial value of buildable bimap type
     */
    protected def initMapVal : Map[ String, Json ] = Map.empty[ String, Json ]

    /**
     * Construct the final type to be bimapped to and from
     *
     * @param buildableValue
     * @return
     */
    protected def buildMapVal( buildableValue : Map[ String, Json ] ) : Json = {
        Json.obj( buildableValue.toSeq: _* )
    }

    protected def extractField[ T ]( from : Json, informedBy : TranslatedFieldDescription[ T, Codec ] ) : T = {
        val valueDecoder = informedBy.schema
        valueDecoder( from.hcursor.downField( informedBy.fieldName ).focus.get.hcursor )
          .getOrElse( throw new Exception() )
    }

    protected def extractAdditionalFields[ T ]( from : Json, informedBy : Codec[ T ] ) : Map[ String, T ] = {
        given Decoder[ T ] = informedBy
        from.as[ Map[ String, T ] ].getOrElse( throw new Exception() )
    }

    protected def writeField[ T ]( value : T, to : Map[ String, Json ], informedBy : TranslatedFieldDescription[ T, Codec ] ) : Map[ String, Json ] = {
        val json = informedBy.schema( value )
        to + (informedBy.fieldName -> json)
    }

    protected def writeAdditionalFields[ T ]( from : Map[ String, T ], to : Map[ String, Json ], informedBy : Codec[ T ] ) : Map[ String, Json ] = {
        val encoder = informedBy
        from.foldLeft( to )( ( lastMap, currentField ) => {
            val (fieldName, fieldValue) = currentField
            lastMap + (fieldName -> encoder( fieldValue ) )
        } )
    }


}
