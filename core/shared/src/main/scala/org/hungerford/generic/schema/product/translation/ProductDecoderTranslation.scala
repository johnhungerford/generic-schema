package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.ProductConstructor
import org.hungerford.generic.schema.product.field.{Field, LazyField}
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator, TransRetriever}

import scala.collection.mutable
import scala.compiletime.{erasedValue, error, summonInline}
import scala.util.Try

trait ProductDecoderTranslation[ DecoderSch[ _ ], Source ]
  extends WithFieldReader[ DecoderSch, Source ] {

    def buildProductDecoder[ T ]( decode : Source => Option[ T ] ) : DecoderSch[ T ]

    def getFieldNames( from : Source ) : Set[ String ]

    def getField[ AF ]( from : Source, fieldName : String, using : DecoderSch[ AF ] ) : Option[ AF ]

    inline given translateProductReaderRecursive[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Trans <: Tuple ](
        using
        pc : ProductConstructor[ C, RV, AF, T ],
    ) : RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Trans, DecoderSch ] = {
        type NextTrans = RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Trans, DecoderSch ] *: Trans
        val fieldsReader: (Source, R, NextTrans) => RV = readFields[ R, RV, NextTrans ]
        val afReader: (Schema.Aux[ AF, AFS ], Source, Set[ String ]) => Map[ String, AF ] =
            readAdditionalFields[ AF, AFS ]

        new RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Trans, DecoderSch ] { self =>
            override def translate(
                schema: Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
                trans: Trans,
            ): DecoderSch[ T ] =
                buildProductDecoder { source =>
                    val cons = pc.construct( schema.shape.constructor )
                    ( Try {
                        val fieldsTuple: RV = fieldsReader( source, schema.shape.fieldDescriptions, self *: trans )
                        val afMap: Map[ String, AF ] = afReader( schema.shape.additionalFieldsSchema, source, schema.shape.fieldNames )

                        cons( fieldsTuple, afMap )
                    } ).toOption
                }
        }
    }

    inline def readAdditionalFields[ AF, AFS ] : (Schema.Aux[ AF, AFS ], Source, Set[ String ]) => Map[ String, AF ] = {
        inline erasedValue[ AF ] match {
            case _ : Nothing =>
                (_: Schema.Aux[ AF, AFS ], _: Source, _: Set[ String ]) => {
                    Map.empty[ String, AF ]
                }
            case _ =>
                val tr = summonInline[ RecursiveSchemaTranslator[ AF, AFS, EmptyTuple, DecoderSch ] ]
                (schema: Schema.Aux[ AF, AFS ], source: Source, usedFieldNames: Set[ String ]) => {
                    val decoder : DecoderSch[ AF ] = tr.translate( schema, EmptyTuple )
                    val fieldNames : Set[ String ] = getFieldNames( source ).diff( usedFieldNames )

                    fieldNames
                      .map( fn => fn -> getField( source, fn, decoder ) )
                      .filter( _._2.nonEmpty )
                      .collect {
                          case (fn, Some( v )) => fn -> v
                      }
                      .toMap
                }
        }
    }

    inline def readFields[ FS <: Tuple, Res <: Tuple, Trans <: Tuple ] : (Source, FS, Trans) => Res = {
        inline erasedValue[FS] match {
            case _: EmptyTuple =>
                inline erasedValue[ Res ] match {
                    case _ : EmptyTuple => (_: Source, _: FS, _: Trans) => EmptyTuple.asInstanceOf[ Res ]
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
                                val fieldTrans = summonInline[ RecursiveSchemaTranslator[ ResHead, S, Trans, DecoderSch ] ]
                                val nextFieldsReader = readFields[Tail, ResTail, Trans ]
                                (fromSource: Source, fields: FS, trans: Trans) => {
                                    val field : Field.Named[ N ] & Field.Shaped[ ResHead, S ]  =
                                        fields.asInstanceOf[*:[Any, Tail]].head
                                          .asInstanceOf[Field.Named[ N ] & Field.Shaped[ ResHead, S ]]
                                    val decoder = fieldTrans.translate( field.schema, trans )

                                    val extraction = getField( fromSource, field.fieldName, decoder ).get
                                    ( extraction *: nextFieldsReader( fromSource, fields.asInstanceOf[ Head *: tail ].tail, trans ) )
                                      .asInstanceOf[ Res ]
                                }

                            case _ : (Field.Named[ n ] & Field.Of[ ResHead]) =>
                                type N = n
                                val fieldName = summonInline[ ValueOf[ N ] ]

                                inline summonInline[ TransRetriever[ Trans, ResHead, DecoderSch ] ] match {
                                    case transRetriever : TransRetriever.Aux[ Trans, ResHead, s, DecoderSch ] =>
                                        type S = s
                                        val fieldSchema = summonInline[ Schema.Aux[ ResHead, S ] ]
                                        val nextFieldsReader = readFields[ Tail, ResTail, Trans ]
                                        (fromSource: Source, fields: FS, trans: Trans) => {
                                            val tr = transRetriever.getTranslator( trans )
                                            val decoder = tr.translate( fieldSchema )
                                            val extraction = getField( fromSource, fieldName.value.asInstanceOf[ String ], decoder ).get
                                            ( extraction *: nextFieldsReader( fromSource, fields.asInstanceOf[ Head *: Tail ].tail, trans ) )
                                              .asInstanceOf[ Res ]
                                        }
                                }
                        }
                }
        }
    }

}
