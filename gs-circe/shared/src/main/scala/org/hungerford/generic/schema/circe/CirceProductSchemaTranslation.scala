package org.hungerford.generic.schema.circe

import io.circe.Decoder.{Result, currencyDecoder}
import org.hungerford.generic.schema.product.field.{Field, FieldName, LazyField}
import org.hungerford.generic.schema.product.translation.BiMapProductTranslation
import io.circe.{ACursor, Codec, Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.ProductConstructor
import org.hungerford.generic.schema.translation.SchemaTranslator

import scala.compiletime.{erasedValue, summonInline, error}
import scala.collection.mutable
import scala.util.Try

trait CirceProductSchemaTranslation {

    inline given translateProductReader[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ](
        using
        pc : ProductConstructor[ C, RV, AF, T ],
    ) : SchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Decoder ] = {
        val fr: (ACursor, R) => RV = readFields[ R, RV ]

        new SchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Decoder ] {
            override def translate(
                schema: Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ]
            ): Decoder[ T ] =
                Decoder.instance { ( cursor: HCursor ) =>
                    val cons = pc.construct( schema.shape.constructor )
                    ( Try {
                        val fieldsTuple: RV = fr( cursor, schema.shape.fieldDescriptions )
                        cons( fieldsTuple, Map.empty )
                    } ).toEither.left.map( e => DecodingFailure.fromThrowable( e, Nil ) )
                }
        }
    }

    inline def readFields[ FS <: Tuple, Res <: Tuple ] : (ACursor, FS) => Res = {
        inline erasedValue[FS] match {
            case _: EmptyTuple =>
                inline erasedValue[ Res ] match {
                    case _ : EmptyTuple => (_: ACursor, EmptyTuple) => EmptyTuple.asInstanceOf[ Res ]
                }
            case _: (head *: tail) =>
                type Head = head
                type Tail = tail
                inline erasedValue[ Res ] match {
                    case _ : EmptyTuple => error(s"FS and Res do not correspond")
                    case _ : (resHead *: resTail) =>
                        type ResHead = resHead
                        type ResTail = resTail
                        inline erasedValue[head] match {
                            case _ : (Field.Named[ n ] & Field.Shaped[ ResHead, s ]) =>
                                type N = n
                                type S = s
                                val fieldName = summonInline[ ValueOf[ N ] ]
                                val fieldTrans = summonInline[ SchemaTranslator[ ResHead, S, Decoder ] ]
                                val nextFieldsReader = readFields[Tail, ResTail]
                                (fromCursor: ACursor, fields: FS) => {
                                    val field : Field.Named[ N ] & Field.Shaped[ ResHead, S ]  =
                                        fields.asInstanceOf[*:[Any, Tail]].head
                                          .asInstanceOf[Field.Named[ N ] & Field.Shaped[ ResHead, S ]]
                                    given Decoder[ ResHead ] = fieldTrans.translate( field.schema )
                                    val extraction = fromCursor.downField( fieldName.value.asInstanceOf[ String ] )
                                      .as[ ResHead ]
                                    ( extraction *: nextFieldsReader( fromCursor, fields.asInstanceOf[ Head *: tail ].tail ) )
                                      .asInstanceOf[ Res ]
                                }

                            case _ : (Field.Named[ n ] & Field.Of[ ResHead]) =>
                                type N = n
                                val fieldName = summonInline[ ValueOf[ N ] ]
                                val fieldSchema = summonInline[ Schema[ ResHead ] ]
                                inline fieldSchema match {
                                    case sch : Schema.Aux[ ResHead, s ] =>
                                        type S = s
                                        val fieldTrans = summonInline[ SchemaTranslator[ ResHead, S, Decoder ] ]
                                        given Decoder[ ResHead ] = fieldTrans.translate( sch )
                                        val nextFieldsReader = readFields[ Tail, ResTail ]
                                        (fromCursor: ACursor, fields: FS) => {
                                            val extraction = fromCursor.downField( fieldName.value.asInstanceOf[ String ] )
                                              .as[ ResHead ]
                                            ( extraction *: nextFieldsReader( fromCursor, fields.asInstanceOf[ Head *: Tail ].tail ) )
                                              .asInstanceOf[ Res ]
                                        }
                                }

                        }
                }
        }
    }

    inline given translateProductWriter[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ] : SchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Encoder ] = {
        val fieldWriter = writeFields[ T, R ]
        val fields : mutable.Map[ String, Json ] = mutable.Map()

        new SchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Encoder ] {
            override def translate(
                schema: Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ]
            ): Encoder[ T ] =
                Encoder.instance { ( value : T ) =>
                    fieldWriter( value, schema.shape.fieldDescriptions, fields )
                    Json.obj( fields.toList : _* )
                }
        }

    }

    // source : T, fields : FS, target : mutable.Map[ String, Json ] => Unit
    inline def writeFields[ T, FS <: Tuple ] : (T, FS, mutable.Map[ String, Json ]) => Unit = {
        inline erasedValue[FS] match {
            case _ : EmptyTuple => (_: T, _: FS, _: mutable.Map[ String, Json ]) => () // don't do anything
            case _ : (headField *: next) =>
                type HeadField = headField
                type Next = next
                inline erasedValue[ HeadField ] match {
                    case _ : Field[ T, f, n, s ] =>
                        type F = f
                        type N = n
                        type S = s
                        val encTrans = summonInline[ SchemaTranslator[ F, S, Encoder ] ]
                        val nextWriter = writeFields[ T, Next ]
                        ( source : T, fields : FS, target : mutable.Map[ String, Json ] ) => {
                            val fs = fields.asInstanceOf[ Field[ T, F, N, S ] *: Next ]
                            val fh = fs.head
                            val next = fs.tail
                            val enc = encTrans.translate( fh.schema )
                            val fieldVal = fh.extractor( source )
                            val fieldJson = enc( fieldVal )
                            target += ((fh.fieldName, fieldJson))
                            nextWriter( source, next, target )
                        }

                    case _ : LazyField[ T, f, n ] =>
                        type F = f
                        type N = n
                        val schema = summonInline[ Schema[ f ] ]
                        inline schema match {
                            case sch : Schema.Aux[ F, s ] =>
                                type S = s
                                val encTrans = summonInline[ SchemaTranslator[ F, S, Encoder ] ]
                                val nextWriter = writeFields[ T, Next ]
                                ( source : T, fields : FS, target : mutable.Map[ String, Json ] ) => {
                                    val fs = fields.asInstanceOf[ Field[ T, F, N, S ] *: Next ]
                                    val fh = fs.head
                                    val next = fs.tail
                                    val enc = encTrans.translate( sch )
                                    val fieldVal = fh.extractor( source )
                                    val fieldJson = enc( fieldVal )
                                    target += ((fh.fieldName, fieldJson))
                                    nextWriter( source, next, target )
                                }
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

    protected def codecFromEncoderDecoder[ T ]( to : T => Json, from : Json => T ) : Codec[ T ] = {
        val encoder : Encoder[ T ] = ( a : T ) => to( a )

        val decoder : Decoder[ T ] = ( c: HCursor ) => {
            Right( from( c.value ) )
        }

        Codec.from( decoder, encoder )
    }

}
