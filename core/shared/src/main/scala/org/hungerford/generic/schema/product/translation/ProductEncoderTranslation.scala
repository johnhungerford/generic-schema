package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, LazyField}
import org.hungerford.generic.schema.translation.{TypeCache, CI, RecursiveSchemaTranslator, TypeCacheRetriever, SchemaTranslator}

import scala.collection.mutable
import scala.compiletime.{erasedValue, error, summonInline}

trait ProductEncoderTranslation[ EncoderSch[ _ ], Sink ]
  extends WithFieldWriter[ EncoderSch, Sink ] {

    def buildProductEncoder[ T ]( encoder : T => Sink ) : EncoderSch[ T ]

    def mergeFields[ T ]( fieldMap : List[ (String, Sink) ] ) : Sink

    def writeField[ T ]( value : T, encoder : EncoderSch[ T ] ) : Sink

    inline given translateProductWriter[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Cache <: TypeCache ] : RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Cache, EncoderSch ] = {
        type NextCache = TypeCache.Cached[Cache, T, EncoderSch[ T ]]
        val fieldWriter = writeFields[ T, R, NextCache ]
        val afWriter = writeAdditionalFields[ T, AF, AFS, AFE ]

        new RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Cache, EncoderSch ] { self =>
            override def translate(
                schema: Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
                cache: Cache,
            ): EncoderSch[ T ] =
                // need to track insertion order bc mutable.ListMap doesn't work!
                lazy val res : EncoderSch[ T ] = buildProductEncoder { ( value : T ) =>
                    val nextCache : NextCache = cache.add[ T ]( res ).asInstanceOf[NextCache]
                    val fields : mutable.Map[ String, (Int, Sink) ] = mutable.Map()
                    fieldWriter( value, schema.shape.fieldDescriptions, fields, nextCache )
                    afWriter( value, schema.shape.afExtractor, schema.shape.additionalFieldsSchema, fields, schema.shape.fieldNames )
                    mergeFields( fields.toList.sortBy( _._2._1 ).map( tp => tp._1 -> tp._2._2 ) )
                }
                res
        }

    }

    inline def writeAdditionalFields[ T, AF, AFS, AFE ] : (T, AFE, Schema.Aux[ AF, AFS ], mutable.Map[ String, (Int, Sink) ], Set[ String ]) => Unit = {
        inline erasedValue[ AF ] match {
            case _ : Nothing =>
                (_: T, _: AFE, _: Schema.Aux[ AF, AFS ], _: mutable.Map[ String, (Int, Sink) ], _: Set[ String ]) => ()
            case _ =>
                inline erasedValue[ AFE ] match {
                    case _ : Function1[ T, Map[ String, AF ] ] =>
                        val tr = summonInline[ SchemaTranslator[ AF, AFS, EncoderSch ] ]
                        (value: T, extr: AFE, schema: Schema.Aux[ AF, AFS ], target: mutable.Map[ String, (Int, Sink) ], usedFields: Set[ String ]) => {
                            val enc = tr.translate( schema )
                            val afs = extr.asInstanceOf[ Function1[ T, Map[ String, AF ] ] ]( value )
                            afs.foreach { case (key, afValue) =>
                                if ( !usedFields.contains( key ) ) target(key) = (target.size, writeField[ AF ]( afValue, enc ))
                            }
                        }
                    case _ => error("Invalid extractor" )
                }

        }
    }

    // source : T, fields : FS, target : mutable.Map[ String, Sink ] => Unit
    inline def writeFields[ T, FS <: Tuple, Cache <: TypeCache ] : (T, FS, mutable.Map[ String, (Int, Sink) ], Cache) => Unit = {
        inline erasedValue[FS] match {
            case _ : EmptyTuple => (_: T, _: FS, _: mutable.Map[ String, (Int, Sink) ], _: Cache) => () // don't do anything
            case _ : (headField *: next) =>
                type HeadField = headField
                type Next = next
                val nextWriter = writeFields[ T, Next, Cache ]
                inline erasedValue[ HeadField ] match {
                    case _ : Field[ T, f, n, s ] =>
                        type F = f
                        type N = n
                        type S = s
                        val encTrans = summonInline[ RecursiveSchemaTranslator[ F, S, Cache, EncoderSch ] ]
                        ( source : T, fields : FS, target : mutable.Map[ String, (Int, Sink) ], cache: Cache ) => {
                            val fs = fields.asInstanceOf[ Field[ T, F, N, S ] *: Next ]
                            val fh = fs.head
                            val next = fs.tail
                            val enc = encTrans.translate( fh.schema, cache )
                            val fieldVal = fh.extractor( source )
                            val fieldSink = writeField[ F ]( fieldVal, enc )
                            target += ((fh.fieldName, (target.size, fieldSink)))
                            nextWriter( source, next, target, cache )
                        }

                    case _ : LazyField[ T, f, n ] =>
                        type F = f
                        type N = n

                        val rt = summonInline[ TypeCacheRetriever.Aux[ Cache, F, EncoderSch[ F ] ] ]

                        ( source : T, fields : FS, target : mutable.Map[ String, (Int, Sink) ], cache: Cache ) => {
                            val fs = fields.asInstanceOf[ LazyField[ T, F, N ] *: Next ]
                            val fh = fs.head
                            val next = fs.tail
                            val enc = rt.get( cache ).get()
                            val fieldVal = fh.extractor( source )
                            val fieldSink = writeField[ F ]( fieldVal, enc )
                            target += ((fh.fieldName, (target.size, fieldSink)))
                            nextWriter( source, next, target, cache )
                        }
                }
        }
    }

}
