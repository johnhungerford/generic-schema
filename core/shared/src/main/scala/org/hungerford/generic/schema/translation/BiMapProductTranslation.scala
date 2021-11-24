package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{TranslatedFieldDescription, TranslatedFieldInjector, FieldTupleTranslator, FieldTranslator}
import org.hungerford.generic.schema.types.{Extractor, Injector, SimpleExtractor}
import org.hungerford.generic.schema.{NoSchema, Primitive, Schema}

import scala.language.higherKinds

import scala.compiletime.{erasedValue, summonInline}
import javax.print.attribute.standard.MediaSize.Other
import org.hungerford.generic.schema.product.CtxWrapTuplesConstraint
import org.hungerford.generic.schema.product.field.FieldDescription

trait BiMapProductTranslation[ OtherSchema[ _ ], MapVal, BuildMapVal ] {

   /**
    * Construct a schema from the two parts of a bimap.
    *
    * @param to   T => MapVal : writer
    * @param from MapVal => T : reader
    * @tparam T type being read/written
    * @return type class instance for some reader/writer
    */
   protected def schemaFromBimap[ T ]( to : T => MapVal, from : MapVal => T ) : OtherSchema[ T ]

   /**
    * Initial empty value for the type being mapped to and from, that can be built
    * by adding field values. For instance, if the value type is Map[ String, T ],
    * initMapVal would be Map.empty[ String, T ]
    *
    * @return initial value of buildable bimap type
    */
   protected def initMapVal : BuildMapVal

   /**
    * Construct the final type to be bimapped to and from
    *
    * @param buildableValue
    * @return
    */
   protected def buildMapVal( buildableValue : BuildMapVal ) : MapVal

   protected def extractField[ T ]( from : MapVal, informedBy : TranslatedFieldDescription[ T, OtherSchema ] ) : T

   protected def extractAdditionalFields[ T ]( from : MapVal, informedBy : OtherSchema[ T ] ) : Map[ String, T ]

   protected def writeField[ T ]( value : T, to : BuildMapVal, informedBy : TranslatedFieldDescription[ T, OtherSchema ] ) : BuildMapVal

   protected def writeAdditionalFields[ T ]( from : Map[ String, T ], to : BuildMapVal, informedBy : OtherSchema[ T ] ) : BuildMapVal

   trait BiMapInjector[ T ] {
       def inject( value : T, into : BuildMapVal, using : TranslatedFieldDescription[ T, OtherSchema ] ) : BuildMapVal
   }

   object BiMapInjector {
       given [ T ] : BiMapInjector[ T ] with {
           def inject( value : T, into : BuildMapVal, informedBy : TranslatedFieldDescription[ T, OtherSchema ] ) : BuildMapVal = {
               writeField[ T ]( value, into, informedBy )
           }
       }
   }

   trait BiMapTupleInjector[ RV <: Tuple, R <: Tuple ] {
       def inject( value : RV, into : BuildMapVal, informedBy : R ) : BuildMapVal
   }

   object BiMapTupleInjector {
       given BiMapTupleInjector[ EmptyTuple, EmptyTuple ] with {
           def inject( value : EmptyTuple, into : BuildMapVal, informedBy : EmptyTuple ) : BuildMapVal = into
       }

       given [ T, S, RVTail <: Tuple, RTail <: Tuple ](
           using
           trans : FieldTranslator[ T, S, OtherSchema ],
           headInjector : BiMapInjector[ T ],
           tailInjector : BiMapTupleInjector[ RVTail, RTail ],
       ) : BiMapTupleInjector[ T *: RVTail, FieldDescription.AuxS[ T, S ] *: RTail ] with {
           def inject( value : T *: RVTail, into : BuildMapVal, informedBy : FieldDescription.AuxS[ T, S ] *: RTail ) : BuildMapVal = {
               val transHead = trans.translate( informedBy.head )
               val headInjected = headInjector.inject( value.head, into, transHead )
               tailInjector.inject( value.tail, headInjected, informedBy.tail )
           }
       }
   }

   trait BiMapExtractor[ T ] {
       def extract( from : MapVal, informedBy : TranslatedFieldDescription[ T, OtherSchema ] ) : T
   }

   object BiMapExtractor {
       given [ T ] : BiMapExtractor[ T ] with {
           def extract( from : MapVal, informedBy : TranslatedFieldDescription[ T, OtherSchema ] ) : T = {
               extractField[ T ]( from, informedBy )
           }
       }
   }

   trait BiMapTupleExtractor[ R <: Tuple ] {
       type Out <: Tuple

       def extract( from : MapVal, informedBy : R ) : Out
   }

   object BiMapTupleExtractor {
        type Aux[ R <: Tuple, O <: Tuple ] = BiMapTupleExtractor[ R ] { type Out = O }

       given BiMapTupleExtractor[ EmptyTuple ] with {
           type Out = EmptyTuple

           def extract( from : MapVal, informedBy : EmptyTuple ) : EmptyTuple = EmptyTuple
       }

       given [ T, S, Tail <: Tuple, TailRes <: Tuple ](
           using
           trans : FieldTranslator[ T, S, OtherSchema ],
           headExtr : BiMapExtractor[ T ],
           tailExtr : BiMapTupleExtractor.Aux[ Tail, TailRes ],
       ) : BiMapTupleExtractor.Aux[ FieldDescription.AuxS[ T, S ] *: Tail, T *: TailRes ] = new BiMapTupleExtractor[ FieldDescription.AuxS[ T, S ] *: Tail ] {
            type Out = T *: TailRes

            def extract( from : MapVal, informedBy : FieldDescription.AuxS[ T, S ] *: Tail ) : T *: tailExtr.Out = {
                val translatedHead = trans.translate( informedBy.head )
                headExtr.extract( from, translatedHead ) *: tailExtr.extract( from, informedBy.tail )
            }
       }
   }

   given fieldInjector[ T ] : TranslatedFieldInjector[ T, BuildMapVal, OtherSchema ] = {
       new TranslatedFieldInjector[ T, BuildMapVal, OtherSchema ] {
           def inject( field : TranslatedFieldDescription[ T, OtherSchema ], value : T, into : BuildMapVal ) : BuildMapVal =
               writeField( value, into, field )
       }
    }

   given additionalFieldsExtractor[ T ] : SimpleExtractor.Aux[ MapVal, OtherSchema[ T ], Map[ String, T ] ] = {
       new SimpleExtractor[ MapVal, OtherSchema[ T ] ] {
           override type Out = Map[ String, T ]

           override def extract( from : MapVal, informedBy : OtherSchema[ T ] ) : Out = extractAdditionalFields( from, informedBy )
       }
   }

    given additionalFieldsInjector[ T ] : Injector.Aux[ Map[ String, T ], BuildMapVal, OtherSchema[ T ], BuildMapVal ] =
       Injector.simpleInjector[ Map[ String, T ], BuildMapVal, OtherSchema[ T ] ] {
           ( value : Map[ String, T ], target : BuildMapVal, using : OtherSchema[ T ] ) =>
               writeAdditionalFields( value, target, using )
       }

    
    type Ctx = [X] =>> TranslatedFieldDescription[ X, OtherSchema ]

    // inline def writerFromProduct[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt ](
    //     product : ProductShape[ T, Rt, RVt, AFt, AFSt ],
    // )(
    //     using
    //     ftt : FieldTupleTranslator.Aux[ Rt, OtherSchema, FDL ],
    //     cx : CtxWrapTuplesConstraint[ Ctx, FDL, RVt ],
    // ) : T => MapVal = { ( value : T ) =>
    //     val (fields, additionalFields) = product.deconstructor( value )
    //     val translatedFieldDescriptions = FieldTupleTranslator.translateFieldDescriptions[ Rt, OtherSchema ]( product.fieldDescriptions )
    //     val buildMapWithFields : BuildMapVal = TranslatedFieldInjector.inject[ FDL, RVt, BuildMapVal, OtherSchema ]( translatedFieldDescriptions, fields, initMapVal )

    //     inline erasedValue[ AFt ] match {
    //         case _ : Nothing =>
    //             buildMapVal( buildMapWithFields )
    //         case _ =>
    //             val afInjector = summonInline[ Injector.Aux[ Map[ String, AFt ], BuildMapVal, OtherSchema[ AFt ], BuildMapVal ] ]
    //             val afTranslator = summonInline[ SchemaTranslator[ AFt, AFSt, OtherSchema ] ]
    //             val afSchema = afTranslator.translate( product.additionalFieldsSchema )
    //             val fieldsSet : Set[ String ] = ???
    //             val fixedAdditionalFields = additionalFields.filter( v => !fieldsSet.contains( v._1 ) )
    //             val buildMapWithAdditionalFields = afInjector.inject( fixedAdditionalFields, buildMapWithFields, afSchema)

    //             buildMapVal( buildMapWithAdditionalFields )
    //     }
    // }

    given encoderWithoutAF[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple ](
        using
        ctx : CtxWrapTuplesConstraint[ Ctx, FDL, RVt ],
        inj : BiMapTupleInjector[ RVt, Rt ],
    ) : Encoder[ T, ProductShape[ T, Rt, RVt, Nothing, Unit ], MapVal ] with {
        def encode( value : T, product : ProductShape[ T, Rt, RVt, Nothing, Unit ] ): MapVal = {
            val (fields, _) = product.deconstructor( value )
            val buildMapWithFields : BuildMapVal = inj.inject( fields, initMapVal, product.fieldDescriptions )
            buildMapVal( buildMapWithFields )
        }
    }

    given encoderWithAF[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt ](
        using
        ctx : CtxWrapTuplesConstraint[ Ctx, FDL, RVt ],
        inj : BiMapTupleInjector[ RVt, Rt ],
        afInjector : Injector.Aux[ Map[ String, AFt ], BuildMapVal, OtherSchema[ AFt ], BuildMapVal ],
        afTranslator : SchemaTranslator[ AFt, AFSt, OtherSchema ],
    ) : Encoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], MapVal ] with {
        def encode( value : T, product : ProductShape[ T, Rt, RVt, AFt, AFSt ] ): MapVal = {
            val (fields, additionalFields) = product.deconstructor( value )
            val buildMapWithFields : BuildMapVal = inj.inject( fields, initMapVal, product.fieldDescriptions )
            val afSchema = afTranslator.translate( product.additionalFieldsSchema )
            val buildMapWithAF : BuildMapVal = afInjector.inject( additionalFields, buildMapWithFields, afSchema )
            buildMapVal( buildMapWithAF )
        }
    }

    // given encoderWithAF[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt, FDL <: Tuple ](
    //     using
    //     ctx : CtxWrapTuplesConstraint[ Ctx, FDL, RVt ],
    //     ftt : FieldTupleTranslator.Aux[ Rt, OtherSchema, FDL ],
    //     afInjector : Injector.Aux[ Map[ String, AFt ], BuildMapVal, OtherSchema[ AFt ], BuildMapVal ],
    //     afTranslator : SchemaTranslator[ AFt, AFSt, OtherSchema ],
    // ) : Encoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], MapVal ] with {
    //     def encode( value : T, product : ProductShape[ T, Rt, RVt, AFt, AFSt ] ): MapVal = {
    //         val (fields, additionalFields) = product.deconstructor( value )
    //         val translatedFieldDescriptions = ftt.translate( product.fieldDescriptions )
    //         val buildMapWithFields : BuildMapVal = TranslatedFieldInjector.inject[ FDL, RVt, BuildMapVal, OtherSchema ]( translatedFieldDescriptions, fields, initMapVal )

    //         val afSchema = afTranslator.translate( product.additionalFieldsSchema )
    //         val fieldsSet : Set[ String ] = ???
    //         val fixedAdditionalFields = additionalFields.filter( v => !fieldsSet.contains( v._1 ) )
    //         val buildMapWithAdditionalFields = afInjector.inject( fixedAdditionalFields, buildMapWithFields, afSchema)

    //         buildMapVal( buildMapWithAdditionalFields )
    //     }
    // }

    // inline given decoderWithAF[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt ](
    //     using
    //     ex : BiMapTupleExtractor.Aux[ Rt, RVt ],
    //     afExtractor : SimpleExtractor.Aux[ MapVal, OtherSchema[ AFt ], Map[ String, AFt ] ],
    //     afTranslator : SchemaTranslator[ AFt, AFSt, OtherSchema ],
    // ) : Decoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], MapVal ] with {
    //     def decode( value : MapVal, product : ProductShape[ T, Rt, RVt, AFt, AFSt ] ) : T = {
    //         val fieldValues = ex.extract( value, product.fieldDescriptions )

    //         val afSchema = afTranslator.translate( product.additionalFieldsSchema )
    //         val additionalFieldValues : Map[ String, AFt ] = afExtractor.extract( value, afSchema )

    //         product.construct( fieldValues, additionalFieldValues )
    //     }
    // }

    given decoderWithAF[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt ](
        using
        ex : BiMapTupleExtractor.Aux[ Rt, RVt ],
        afExtractor : SimpleExtractor.Aux[ MapVal, OtherSchema[ AFt ], Map[ String, AFt ] ],
        afTranslator : SchemaTranslator[ AFt, AFSt, OtherSchema ],
    ) : Decoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], MapVal ] with {
        def decode( value : MapVal, product : ProductShape[ T, Rt, RVt, AFt, AFSt ] ) : T = {
            val fieldValues = ex.extract( value, product.fieldDescriptions )
            val afSchema = afTranslator.translate( product.additionalFieldsSchema )
            val additionalFieldValues = afExtractor.extract( value, afSchema )
            product.construct( fieldValues, additionalFieldValues )
        }
    }

    given decoderWithoutAF[ T, Rt <: Tuple, RVt <: Tuple ](
        using
        ex : BiMapTupleExtractor.Aux[ Rt, RVt ],
    ) : Decoder[ T, ProductShape[ T, Rt, RVt, Nothing, Unit ], MapVal ] with {
        def decode( value : MapVal, product : ProductShape[ T, Rt, RVt, Nothing, Unit ] ) : T = {
            val fieldValues = ex.extract( value, product.fieldDescriptions )
            product.construct( fieldValues )
        }
    }


    // inline def readerFromProduct[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt ](
    //     product : ProductShape[ T, Rt, RVt, AFt, AFSt ],
    // )(
    //     using
    //     ftt : FieldTupleTranslator.Aux[ Rt, OtherSchema, FDL ],
    //     cx : CtxWrapTuplesConstraint[ Ctx, FDL, RVt ],
    //     ex : FieldsExtractor[MapVal, FDL],
    // ) : MapVal => T = { ( mapVal : MapVal ) =>
    //     val translatedFieldDescriptions = FieldTupleTranslator.translateFieldDescriptions[ Rt, OtherSchema ]( product.fieldDescriptions )
    //     val fieldValues = FieldsExtractor.extract[ MapVal, FDL ]( mapVal, translatedFieldDescriptions )
        
    //     inline fieldValues match {
    //         case correctFV : RVt =>
    //             inline erasedValue[ AFt ] match {
    //                 case _: Nothing => 
    //                     product.construct( correctFV, Map.empty[ String, AFt ] )
    //                 case _ =>
    //                     val afExtractor = summonInline[ SimpleExtractor.Aux[ MapVal, OtherSchema[ AFt ], Map[ String, AFt ] ] ]
    //                     val afTranslator = summonInline[ SchemaTranslator[ AFt, AFSt, OtherSchema ] ]
    //                     val afSchema = afTranslator.translate( product.additionalFieldsSchema )
    //                     val additionalFieldValues : Map[ String, AFt ] = afExtractor.extract( mapVal, afSchema )
    //                     product.construct( correctFV, additionalFieldValues )
    //             }
                
    //     }
        
    // }


    // inline given productTranslation[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt ](
    //     using
    //     ftt : FieldTupleTranslator.Aux[ Rt, OtherSchema, FDL ],
    //     cx : CtxWrapTuplesConstraint[ Ctx, FDL, RVt ],
    //     ex : FieldsExtractor[MapVal, FDL],
    // ) : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], OtherSchema ] = {
    //         new SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], OtherSchema ] {
    //             def translate( schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ] ] ) : OtherSchema[ T ] = {
    //                 val writer : T => MapVal = writerFromProduct[ T, Rt, RVt, FDL, AFt, AFSt ]( schema.shape )
    //                 val reader : MapVal => T = readerFromProduct[ T, Rt, RVt, FDL, AFt, AFSt ]( schema.shape )
    //                 schemaFromBimap( writer, reader )                    
    //             }
    //         }
    //     }

    // inline given productTranslationWithAF[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt ](
    //     using
    //     enc : Encoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], MapVal ],
    //     dec : Decoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], MapVal ],
    // ) : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], OtherSchema ] = {
    //         new SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], OtherSchema ] {
    //             def translate( schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ] ] ) : OtherSchema[ T ] = {
    //                 val writer : T => MapVal = ( value : T ) => enc.encode( value, schema.shape )
    //                 val reader : MapVal => T = ( value : MapVal ) => dec.decode( value, schema.shape )
    //                 schemaFromBimap( writer, reader )                    
    //             }
    //         }
    //     }

    // inline given productTranslationWithoutAF[ T, Rt <: Tuple, RVt <: Tuple ](
    //     using
    //     enc : Encoder[ T, ProductShape[ T, Rt, RVt, Nothing, Unit ], MapVal ],
    //     dec : Decoder[ T, ProductShape[ T, Rt, RVt, Nothing, Unit ], MapVal ],
    // ) : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, Nothing, Unit ], OtherSchema ] = {
    //         new SchemaTranslator[ T, ProductShape[ T, Rt, RVt, Nothing, Unit ], OtherSchema ] {
    //             def translate( schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, Nothing, Unit ] ] ) : OtherSchema[ T ] = {
    //                 val writer : T => MapVal = ( value : T ) => enc.encode( value, schema.shape )
    //                 val reader : MapVal => T = ( value : MapVal ) => dec.decode( value, schema.shape )
    //                 schemaFromBimap( writer, reader )                    
    //             }
    //         }
    //     }

    inline given productTranslation[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt ](
        using
        enc : Encoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], MapVal ],
        dec : Decoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], MapVal ],
    ) : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], OtherSchema ] = {
            new SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], OtherSchema ] {
                def translate( schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ] ] ) : OtherSchema[ T ] = {
                    val writer : T => MapVal = ( value : T ) => enc.encode( value, schema.shape )
                    val reader : MapVal => T = ( value : MapVal ) => dec.decode( value, schema.shape )
                    schemaFromBimap( writer, reader )                    
                }
            }
        }

    def translate[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt ]( 
        schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ] ],
    )(
        using
        tr : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], OtherSchema ],
    ) : OtherSchema[ T ] = tr.translate( schema )

}
