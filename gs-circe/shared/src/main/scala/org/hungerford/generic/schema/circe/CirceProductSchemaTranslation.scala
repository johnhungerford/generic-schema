package org.hungerford.generic.schema.circe

import io.circe.Decoder.{Result, currencyDecoder}
import org.hungerford.generic.schema.product.field.{Field, FieldName, LazyField}
import org.hungerford.generic.schema.product.translation.BiMapProductTranslation
import io.circe.{Codec, Decoder, Encoder, HCursor, Json, JsonObject}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.translation.SchemaTranslator

import scala.compiletime.summonInline
import scala.collection.mutable

trait CirceProductSchemaTranslation {

    inline def translateProductWriter[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ](
        product : ProductShape[ T, R, RV, AF, AFS, AFE, C ],
    ) : Encoder[ T ] = {
        val fields : mutable.Map[ String, Json ] = mutable.Map()
        Encoder.instance { ( value : T ) =>
            processFields( value, product.fieldDescriptions, fields )
            Json.obj( fields.toList : _* )
        }
    }

    inline def processFields[ T, FS <: Tuple ]( source : T, fields : FS, target : mutable.Map[ String, Json ] ) : Unit = {
        inline fields match {
            case EmptyTuple =>
            case headField *: next =>
                inline headField match {
                    case hf : Field[ T, f, n, s ] =>
                        val enc = summonInline[ SchemaTranslator[ f, s, Encoder ] ].translate( hf.schema )
                        val fieldVal = hf.extractor( source )
                        val fieldJson = enc( fieldVal )
                        target += ((hf.fieldName, fieldJson))
                        processFields( source, next, target )

                    case hlf : LazyField[ T, f, n ] =>
                        val schema = summonInline[ Schema[ f ] ]
                        inline schema match {
                            case sch : Schema.Aux[ f, s ] =>
                                val enc = summonInline[ SchemaTranslator[ f, s, Encoder ] ].translate( sch )
                                val fieldVal = hlf.extractor( source )
                                val fieldJson = enc( fieldVal )
                                target += ((hlf.fieldName, fieldJson))
                                processFields( source, next, target )
                        }
                }
        }
    }

    inline def writeField[ T, F, N <: FieldName, S ](
        field : Field[ T, F, N, S ],
        source : F,
        target : mutable.Map[ String, Json ],
    ) : Unit = {
        val translator = summonInline[ SchemaTranslator[ F, S, Encoder ] ]
        val encoder = translator.translate( field.schema )
        val fieldJson = encoder( source )
        target += ((field.fieldName, fieldJson))
    }

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

    protected def extractField[ T, F ]( from : Json, field : Field.Extr[ T, F ], schema : Codec[ F ] ) : F = {
        schema( from.hcursor.downField( field.fieldName ).focus.get.hcursor )
          .getOrElse( throw new Exception() )
    }

    protected def extractAdditionalFields[ T ]( from : Json, informedBy : Codec[ T ] ) : Map[ String, T ] = {
        given Decoder[ T ] = informedBy
        from.as[ Map[ String, T ] ].getOrElse( throw new Exception() )
    }

    protected def writeField[ F ]( value : F, to : Map[ String, Json ], field : Field.Of[ F ], schema : Codec[ F ] ) : Map[ String, Json ] = {
        val json = schema( value )
        to + (field.fieldName -> json)
    }

    protected def writeAdditionalFields[ T ]( from : Map[ String, T ], to : Map[ String, Json ], informedBy : Codec[ T ] ) : Map[ String, Json ] = {
        val encoder = informedBy
        from.foldLeft( to )( ( lastMap, currentField ) => {
            val (fieldName, fieldValue) = currentField
            lastMap + (fieldName -> encoder( fieldValue ) )
        } )
    }


}
