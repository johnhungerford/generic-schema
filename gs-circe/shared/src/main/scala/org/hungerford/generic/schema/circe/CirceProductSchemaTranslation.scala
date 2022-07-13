package org.hungerford.generic.schema.circe

import io.circe.Decoder.{Result, currencyDecoder}
import org.hungerford.generic.schema.product.field.{Field, FieldName, LazyField}
import org.hungerford.generic.schema.product.translation.BiMapProductTranslation
import io.circe.{ACursor, Codec, Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.ProductConstructor
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator}

import scala.compiletime.{erasedValue, error, summonFrom, summonInline}
import scala.collection.mutable
import scala.util.Try

trait CirceProductSchemaTranslation {

    inline given translateProductReaderRecursive[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Trans <: Tuple ](
        using
        pc : ProductConstructor[ C, RV, AF, T ],
    ) : RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Trans, Decoder ] = {
        type NextTrans = RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Trans, Decoder ] *: Trans
        val fieldsReader: (ACursor, R, NextTrans) => RV = readFields[ R, RV, NextTrans ]
        val afReader: (Schema.Aux[ AF, AFS ], ACursor, Set[ String ]) => Map[ String, AF ] =
            readAdditionalFields[ AF, AFS ]

        new RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Trans, Decoder ] { self =>
            override def translate(
                schema: Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
                trans: Trans,
            ): Decoder[ T ] =
                Decoder.instance { ( cursor: HCursor ) =>
                    val cons = pc.construct( schema.shape.constructor )
                    ( Try {
                        val fieldsTuple: RV = fieldsReader( cursor, schema.shape.fieldDescriptions, self *: trans )
                        val afMap: Map[ String, AF ] = afReader( schema.shape.additionalFieldsSchema, cursor, schema.shape.fieldNames )

                        cons( fieldsTuple, afMap )
                    } ).toEither.left.map( e => DecodingFailure.fromThrowable( e, Nil ) )
                }
        }
    }

    inline def readAdditionalFields[ AF, AFS ] : (Schema.Aux[ AF, AFS ], ACursor, Set[ String ]) => Map[ String, AF ] = {
        inline erasedValue[ AF ] match {
            case _ : Nothing =>
                (_: Schema.Aux[ AF, AFS ], _: ACursor, _: Set[ String ]) => {
                    Map.empty[ String, AF ]
                }
            case _ =>
                val tr = summonInline[ RecursiveSchemaTranslator[ AF, AFS, EmptyTuple, Decoder ] ]
                (schema: Schema.Aux[ AF, AFS ], cursor: ACursor, usedFieldNames: Set[ String ]) => {
                    given Decoder[ AF ] = tr.translate( schema, EmptyTuple )
                    cursor.keys match {
                        case None => Map.empty[ String, AF ]
                        case Some(keys) => ( keys.collect {
                            case key if !usedFieldNames.contains(key) =>
                                val afFieldCursor = cursor.downField( key )
                                key -> afFieldCursor.as[ AF ]
                        } ).toMap[ String, Result[ AF ] ].collect {
                            case (key, Right(af)) => key -> af
                        }
                    }
                }
        }
    }

    inline def readFields[ FS <: Tuple, Res <: Tuple, Trans <: Tuple ] : (ACursor, FS, Trans) => Res = {
        inline erasedValue[FS] match {
            case _: EmptyTuple =>
                inline erasedValue[ Res ] match {
                    case _ : EmptyTuple => (_: ACursor, _: FS, _: Trans) => EmptyTuple.asInstanceOf[ Res ]
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
                                val fieldTrans = summonInline[ RecursiveSchemaTranslator[ ResHead, S, Trans, Decoder ] ]
                                val nextFieldsReader = readFields[Tail, ResTail, Trans ]
                                (fromCursor: ACursor, fields: FS, trans: Trans) => {
                                    val field : Field.Named[ N ] & Field.Shaped[ ResHead, S ]  =
                                        fields.asInstanceOf[*:[Any, Tail]].head
                                          .asInstanceOf[Field.Named[ N ] & Field.Shaped[ ResHead, S ]]
                                    given Decoder[ ResHead ] = fieldTrans.translate( field.schema, trans )
                                    val extraction = fromCursor.downField( fieldName.value.asInstanceOf[ String ] )
                                      .as[ ResHead ] match {
                                        case Left( e ) => throw e
                                        case Right( v ) => v
                                    }
                                    ( extraction *: nextFieldsReader( fromCursor, fields.asInstanceOf[ Head *: tail ].tail, trans ) )
                                      .asInstanceOf[ Res ]
                                }

                            case _ : (Field.Named[ n ] & Field.Of[ ResHead]) =>
                                type N = n
                                val fieldName = summonInline[ ValueOf[ N ] ]
                                val fieldSchema = summonInline[ Schema[ ResHead ] ]

                                // DANGER WILL ROBINSON!
                                // Assuming that given schema has same shape as the cached translator
                                // (never able to prove -- why??)
                                val transRetriever = summonInline[ TransRetriever[ Trans, ResHead, Decoder ] ]
                                  .asInstanceOf[ TransRetriever.Aux[ Trans, ResHead, fieldSchema.Shape, Decoder ] ]

                                val nextFieldsReader = readFields[ Tail, ResTail, Trans ]
                                (fromCursor: ACursor, fields: FS, trans: Trans) => {
                                    val tr = transRetriever.getTranslator( trans )

                                    given Decoder[ ResHead ] = tr.translate( fieldSchema )
                                    val extraction = fromCursor.downField( fieldName.value.asInstanceOf[ String ] )
                                      .as[ ResHead ] match {
                                        case Left( e ) => throw e
                                        case Right( v ) => v
                                    }
                                    ( extraction *: nextFieldsReader( fromCursor, fields.asInstanceOf[ Head *: Tail ].tail, trans ) )
                                      .asInstanceOf[ Res ]
                                }
                        }
                }
        }
    }

    inline given translateProductWriter[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Trans <: Tuple ] : RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Trans, Encoder ] = {
        type NextTrans = RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Trans, Encoder ] *: Trans
        val fieldWriter = writeFields[ T, R, NextTrans ]
        val afWriter = writeAdditionalFields[ T, AF, AFS, AFE ]
        val fields : mutable.Map[ String, (Int, Json) ] = mutable.Map() // need to track insertion order bc mutable.ListMap doesn't work!

        new RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Trans, Encoder ] { self =>
            override def translate(
                schema: Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
                translators: Trans,
            ): Encoder[ T ] =
                Encoder.instance { ( value : T ) =>
                    fieldWriter( value, schema.shape.fieldDescriptions, fields, self *: translators )
                    afWriter( value, schema.shape.afExtractor, schema.shape.additionalFieldsSchema, fields, schema.shape.fieldNames )
                    Json.obj( fields.toList.sortBy( _._2._1 ).map( tp => tp._1 -> tp._2._2 ) : _* )
                }
        }

    }

    inline def writeAdditionalFields[ T, AF, AFS, AFE ] : (T, AFE, Schema.Aux[ AF, AFS ], mutable.Map[ String, (Int, Json) ], Set[ String ]) => Unit = {
        inline erasedValue[ AF ] match {
            case _ : Nothing =>
                (_: T, _: AFE, _: Schema.Aux[ AF, AFS ], _: mutable.Map[ String, (Int, Json) ], _: Set[ String ]) => ()
            case _ =>
                inline erasedValue[ AFE ] match {
                    case _ : Function1[ T, Map[ String, AF ] ] =>
                        val tr = summonInline[ SchemaTranslator[ AF, AFS, Encoder ] ]
                        (value: T, extr: AFE, schema: Schema.Aux[ AF, AFS ], target: mutable.Map[ String, (Int, Json) ], usedFields: Set[ String ]) => {
                            val enc = tr.translate( schema )
                            val afs = extr.asInstanceOf[ Function1[ T, Map[ String, AF ] ] ]( value )
                            afs.foreach { case (key, afValue) =>
                                if ( !usedFields.contains( key ) ) target(key) = (target.size, enc( afValue ))
                            }
                        }
                    case _ => error("Invalid extractor" )
                }

        }
    }

    // source : T, fields : FS, target : mutable.Map[ String, Json ] => Unit
    inline def writeFields[ T, FS <: Tuple, Trans <: Tuple ] : (T, FS, mutable.Map[ String, (Int, Json) ], Trans) => Unit = {
        inline erasedValue[FS] match {
            case _ : EmptyTuple => (_: T, _: FS, _: mutable.Map[ String, (Int, Json) ], _: Trans) => () // don't do anything
            case _ : (headField *: next) =>
                type HeadField = headField
                type Next = next
                inline erasedValue[ HeadField ] match {
                    case _ : Field[ T, f, n, s ] =>
                        type F = f
                        type N = n
                        type S = s
                        val encTrans = summonInline[ RecursiveSchemaTranslator[ F, S, Trans, Encoder ] ]
                        val nextWriter = writeFields[ T, Next, Trans ]
                        ( source : T, fields : FS, target : mutable.Map[ String, (Int, Json) ], trans: Trans ) => {
                            val fs = fields.asInstanceOf[ Field[ T, F, N, S ] *: Next ]
                            val fh = fs.head
                            val next = fs.tail
                            val enc = encTrans.translate( fh.schema, trans )
                            val fieldVal = fh.extractor( source )
                            val fieldJson = enc( fieldVal )
                            target += ((fh.fieldName, (target.size, fieldJson)))
                            nextWriter( source, next, target, trans )
                        }

                    case _ : LazyField[ T, f, n ] =>
                        type F = f
                        type N = n
                        val schema = summonInline[ Schema[ f ] ]
                        // DANGER WILL ROBINSON!
                        // Assuming that given schema has same shape as the cached translator
                        // (never able to prove -- why??)
                        val transRetriever = summonInline[ TransRetriever[ Trans, F, Encoder ] ]
                          .asInstanceOf[ TransRetriever.Aux[ Trans, F, schema.Shape, Encoder ] ]

                        val nextWriter = writeFields[ T, Next, Trans ]
                        ( source : T, fields : FS, target : mutable.Map[ String, (Int, Json) ], trans: Trans ) => {
                            val fs = fields.asInstanceOf[ LazyField[ T, F, N ] *: Next ]
                            val fh = fs.head
                            val next = fs.tail
                            val encTrans = transRetriever.getTranslator( trans )
                            val enc = encTrans.translate( schema )
                            val fieldVal = fh.extractor( source )
                            val fieldJson = enc( fieldVal )
                            target += ((fh.fieldName, (target.size, fieldJson)))
                            nextWriter( source, next, target, trans )
                        }
                }
        }
    }

//    inline def writeField[ T, F, N <: FieldName, S ](
//        field : Field[ T, F, N, S ],
//        source : F,
//        target : mutable.Map[ String, Json ],
//    ) : Unit = {
//        val translator = summonInline[ RecursiveSchemaTranslator[ F, S, Encoder ] ]
//        val encoder = translator.translate( field.schema )
//        val fieldJson = encoder( source )
//        target += ((field.fieldName, fieldJson))
//    }

    protected def codecFromEncoderDecoder[ T ]( to : T => Json, from : Json => T ) : Codec[ T ] = {
        val encoder : Encoder[ T ] = ( a : T ) => to( a )

        val decoder : Decoder[ T ] = ( c: HCursor ) => {
            Right( from( c.value ) )
        }

        Codec.from( decoder, encoder )
    }

}
