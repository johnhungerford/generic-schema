package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.ProductConstructor
import org.hungerford.generic.schema.product.field.{Field, FieldName, LazyField}
import org.hungerford.generic.schema.translation.{CI, RecursiveSchemaTranslator, SchemaTranslator, TypeCache, TypeCacheRetriever}

import scala.collection.mutable
import scala.compiletime.{erasedValue, error, summonInline}
import scala.util.{Success, Try}

trait ProductDecoderTranslation[ DecoderSch[ _ ], Source ]
  extends WithFieldReader[ DecoderSch, Source ] {

    def buildProductDecoder[ T ]( decode : Source => Try[ T ] ) : DecoderSch[ T ]

    def getFieldNames( from : Source ) : Set[ String ]

    def getField[ AF ]( from : Source, fieldName : String, using : DecoderSch[ AF ] ) : Try[ AF ]

    trait FieldsReader[ R <: Tuple, Cache <: TypeCache ] {
        type Out

        def read( from : Source, fieldDescriptions : R, cache : Cache ) : Out
    }

    object FieldsReader {
        type Aux[ R <: Tuple, Cache <: TypeCache, O ] = FieldsReader[ R, Cache ] { type Out = O }

        given emptyFieldsReader[ Cache <: TypeCache ] : FieldsReader[ EmptyTuple, Cache ] with {
            type Out = Try[ EmptyTuple ]
            override def read( from: Source, fieldDescriptions: EmptyTuple, cache: Cache ): Try[ EmptyTuple ] = Success( EmptyTuple )
        }

        given singleFieldReader[ T, F, N <: FieldName, S, Tail <: Tuple, Cache <: TypeCache, NextRV <: Tuple ](
            using
            str : RecursiveSchemaTranslator[ F, S, Cache, DecoderSch ],
            nextReader : FieldsReader.Aux[ Tail, Cache, Try[ NextRV ] ]
        ): FieldsReader[ Field[ T, F, N, S ] *: Tail, Cache ] with {
            type Out = Try[ F *: NextRV ]

            override def read(
                from: Source,
                fieldDescriptions: Field[T, F, N, S] *: Tail,
                cache: Cache
            ): Try[ F *: NextRV ] = {
                val field : Field[T, F, N, S] = fieldDescriptions.head
                val nextFields = fieldDescriptions.tail
                val fieldDecoder = str.translate( field.schema, cache )
                for {
                    fieldValue      <- getField[ F ]( from, field.fieldName, fieldDecoder )
                    nextFieldValues <- nextReader.read( from, nextFields, cache )
                } yield fieldValue *: nextFieldValues
            }
        }

        given singleLazyFieldReader[ T, F, N <: FieldName, Tail <: Tuple, Cache <: TypeCache, NextRV <: Tuple ](
            using
            retriever : TypeCacheRetriever.Aux[ Cache, F, DecoderSch[ F ] ],
            nextReader : FieldsReader.Aux[ Tail, Cache, Try[ NextRV ] ]
        ): FieldsReader[ LazyField[ T, F, N ] *: Tail, Cache ] with {
            type Out = Try[ F *: NextRV ]

            override def read(
                from: Source,
                fieldDescriptions: LazyField[T, F, N] *: Tail,
                cache: Cache
            ): Try[ F *: NextRV ] = {
                val field : LazyField[T, F, N] = fieldDescriptions.head
                val nextFields = fieldDescriptions.tail
                val fieldDecoder = retriever.get(cache).get()
                for {
                    fieldValue      <- getField[ F ]( from, field.fieldName, fieldDecoder )
                    nextFieldValues <- nextReader.read( from, nextFields, cache )
                } yield fieldValue *: nextFieldValues
            }
        }
    }

    trait AFReader[ AF, AFS, Cache <: TypeCache ] {
        def readAf( from : Source, informedBy : Schema.Aux[ AF, AFS ], ignoreFields : Set[ String ], cache : Cache ) : Map[ String, AF ]
    }

    object AFReader {
        given [ AF, AFS, Cache <: TypeCache ](
            using
            aft: RecursiveSchemaTranslator[ AF, AFS, Cache, DecoderSch ]
        ) : AFReader[AF, AFS, Cache] with {
            override def readAf(
                from: Source,
                schema: Schema.Aux[ AF, AFS ],
                ignoreFields: Set[ String ],
                cache: Cache
            ): Map[ String, AF ] = {
                val decoder : DecoderSch[ AF ] = aft.translate( schema, cache )
                val fieldNames : Set[ String ] = getFieldNames( from ).diff( ignoreFields )

                fieldNames
                  .map( fn => fn -> getField( from, fn, decoder ) )
                  .collect {
                      case (fn, Success( v )) => fn -> v
                  }
                  .toMap
            }
        }

        given [ AFS, Cache <: TypeCache ] : AFReader[Nothing, AFS, Cache] with {
            override def readAf(
                from: Source,
                schema: Schema.Aux[ Nothing, AFS ],
                ignoreFields: Set[ String ],
                cache: Cache
            ): Map[ String, Nothing ] = Map.empty
        }
    }


    inline given translateProductReaderRecursive[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Cache <: TypeCache ](
        using
        pc : ProductConstructor[ C, RV, AF, T ],
        fieldsReader : FieldsReader.Aux[ R, TypeCache.Cached[Cache, T, DecoderSch[T]], Try[ RV ] ],
        afReader : AFReader[ AF, AFS, TypeCache.Cached[Cache, T, DecoderSch[T]] ],
    ) : RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Cache, DecoderSch ] = {
        type NextCache = TypeCache.Cached[Cache, T, DecoderSch[T]]

        new RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Cache, DecoderSch ] { self =>
            override def translate(
                schema: Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
                cache: Cache,
            ): DecoderSch[ T ] =
                lazy val res: DecoderSch[ T ] = buildProductDecoder { source =>
                    val cons = pc.construct( schema.shape.constructor )
                    val nextCache: NextCache = cache.add[T]( res ).asInstanceOf[NextCache]
                    val fieldsTuple: Try[RV] =
                        fieldsReader.read( source, schema.shape.fieldDescriptions, nextCache )
                    val afMap: Map[ String, AF ] =
                        afReader.readAf( source, schema.shape.additionalFieldsSchema, schema.shape.fieldNames, nextCache )

                    fieldsTuple.map(ft => cons( ft, afMap ))
                }
                res
        }
    }

//    inline def readAdditionalFields[ AF, AFS ] : (Schema.Aux[ AF, AFS ], Source, Set[ String ]) => Map[ String, AF ] = {
//        inline erasedValue[ AF ] match {
//            case _ : Nothing =>
//                (_: Schema.Aux[ AF, AFS ], _: Source, _: Set[ String ]) => {
//                    Map.empty[ String, AF ]
//                }
//            case _ =>
//                val tr = summonInline[ RecursiveSchemaTranslator[ AF, AFS, TypeCache.Empty, DecoderSch ] ]
//                (schema: Schema.Aux[ AF, AFS ], source: Source, usedFieldNames: Set[ String ]) => {
//                    val decoder : DecoderSch[ AF ] = tr.translate( schema, TypeCache.Empty )
//                    val fieldNames : Set[ String ] = getFieldNames( source ).diff( usedFieldNames )
//
//                    fieldNames
//                      .map( fn => fn -> getField( source, fn, decoder ) )
//                      .filter( _._2.nonEmpty )
//                      .collect {
//                          case (fn, Some( v )) => fn -> v
//                      }
//                      .toMap
//                }
//        }
//    }
//
//    inline def readFields[ FS <: Tuple, Res <: Tuple, Cache <: TypeCache ] : (Source, FS, Cache) => Res = {
//        inline erasedValue[FS] match {
//            case _: EmptyTuple =>
//                inline erasedValue[ Res ] match {
//                    case _ : EmptyTuple => (_: Source, _: FS, _: Cache) => EmptyTuple.asInstanceOf[ Res ]
//                }
//            case _: (head *: tail) =>
//                type Head = head
//                type Tail = tail
//                inline erasedValue[ Res ] match {
//                    case _ : EmptyTuple => error(s"FS and Res do not correspond")
//                    case _ : (resHead *: resTail) =>
//                        type ResHead = resHead
//                        type ResTail = resTail
//                        val nextFieldsReader = readFields[Tail, ResTail, Cache ]
//                        inline erasedValue[head] match {
//                            case _ : (Field.Named[ n ] & Field.Shaped[ ResHead, s ]) =>
//                                type N = n
//                                type S = s
//                                val fieldTrans = summonInline[ RecursiveSchemaTranslator[ ResHead, S, Cache, DecoderSch ] ]
//
//                                (fromSource: Source, fields: FS, cache: Cache) => {
//                                    val field : Field.Named[ N ] & Field.Shaped[ ResHead, S ]  =
//                                        fields.asInstanceOf[*:[Any, Tail]].head
//                                          .asInstanceOf[Field.Named[ N ] & Field.Shaped[ ResHead, S ]]
//                                    val decoder = fieldTrans.translate( field.schema, cache )
//
//                                    val extraction = getField( fromSource, field.fieldName, decoder ).get
//                                    ( extraction *: nextFieldsReader( fromSource, fields.asInstanceOf[ Head *: tail ].tail, cache ) )
//                                      .asInstanceOf[ Res ]
//                                }
//
//                            case _ : (Field.Named[ n ] & Field.Of[ ResHead]) =>
//                                type N = n
//                                val fieldName = summonInline[ ValueOf[ N ] ]
//
//                                val transRetriever = summonInline[ TypeCacheRetriever.Aux[ Cache, ResHead, DecoderSch[ ResHead ] ] ]
//
//                                (fromSource: Source, fields: FS, cache: Cache) => {
//                                    val decoder = transRetriever.get( cache ).get()
//                                    val extraction = getField( fromSource, fieldName.value.asInstanceOf[ String ], decoder ).get
//                                    ( extraction *: nextFieldsReader( fromSource, fields.asInstanceOf[ Head *: Tail ].tail, cache ) )
//                                      .asInstanceOf[ Res ]
//                                }
//                        }
//                }
//        }
//    }

}
