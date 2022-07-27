package org.hungerford.generic.schema.upickle

import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.product.translation.BiMapProductTranslation
import ujson.Value
import upickle.default.*

import scala.collection.immutable.ListMap
import scala.util.Try

trait UPickleProductTranslation
  extends BiMapProductTranslation[ ReadWriter, Reader, Writer, Value.Value, Value.Value ] {

    def buildProductSchema[ T ](
        enc: Writer[ T ], dec: Reader[ T ]
    ): ReadWriter[ T ] = ReadWriter.join( dec, enc )

    def mergeFields[ T ](
        fieldMap: List[ (String, Value.Value) ]
    ): Value.Value =
        fieldMap match {
            case Nil => ujson.Obj()
            case head :: tail => ujson.Obj( head, tail* )
        }

    def getFieldNames(
        from: Value.Value
    ): Set[ String ] = from.obj.keySet.toSet

    def writeField[ T ]( value: T, encoder: Writer[ T ] ): Value.Value =
        given Writer[ T ] = encoder
        writeJs[ T ]( value )

    def getField[ AF ](
        from: Value.Value, fieldName: String, decoder: Reader[ AF ]
    ): Try[ AF ] =
        Try( from( fieldName ) ) flatMap { fieldValue =>
            given Reader[ AF ] = decoder
            Try( read[ AF ]( fieldValue ) )
        }

    def buildProductDecoder[ T ](
        decode: Value.Value => Try[ T ]
    ): Reader[ T ] = reader[ Value.Value ].map( v => decode( v ).get )

    def buildProductEncoder[ T ]( decode: T => Value.Value ): Writer[ T ] =
        readwriter[ Value.Value ].bimap[ T ]( decode, _.asInstanceOf[ T ] )

}

object UPickleProductTranslation extends UPickleProductTranslation
