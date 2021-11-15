package org.hungerford.generic.schema.translation

import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{FieldExtractor, FieldsExtractor, TranslatedFieldDescription, TranslatedFieldInjector, FieldTupleTranslator}
import org.hungerford.generic.schema.types.{Extractor, Injector, SimpleExtractor}
import org.hungerford.generic.schema.{NoSchema, Primitive, Schema}

import scala.language.higherKinds
import org.hungerford.generic.schema.product.field.{FieldTranslator, FieldTupleTranslator, FieldInjector, FieldExtractor}

import scala.compiletime.{erasedValue, summonInline}
import javax.print.attribute.standard.MediaSize.Other
import org.hungerford.generic.schema.product.CtxWrapTuplesConstraint

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

   protected def extractField[ T ]( from : MapVal, using : TranslatedFieldDescription[ T, OtherSchema ] ) : T

   protected def extractAdditionalFields[ T ]( from : MapVal, using : OtherSchema[ T ] ) : Map[ String, T ]

   protected def writeField[ T ]( value : T, to : BuildMapVal, using : TranslatedFieldDescription[ T, OtherSchema ] ) : BuildMapVal

   protected def writeAdditionalFields[ T ]( from : Map[ String, T ], to : BuildMapVal, using : OtherSchema[ T ] ) : BuildMapVal


   given fieldExtractor[ T, Rt ] : FieldExtractor[ MapVal, T, OtherSchema ] with {
       override type Out = T

       override def extract( from : MapVal, informedBy : TranslatedFieldDescription[ T, OtherSchema ] ) : Out = {
           extractField( from, informedBy )
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

    inline def writerFromProduct[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt ](
        product : ProductShape[ T, Rt, RVt, AFt, AFSt ],
    )(
        using
        ftt : FieldTupleTranslator.Aux[ Rt, OtherSchema, FDL ],
        cx : CtxWrapTuplesConstraint[ TFD, FDL, RVt ],
    ) : T => MapVal = { ( value : T ) =>
        val (fields, additionalFields) = product.deconstructor( value )
        val translatedFieldDescriptions = FieldTupleTranslator.translateFieldDescriptions[ Rt, OtherSchema ]( product.fieldDescriptions )
        val buildMapWithFields : BuildMapVal = TranslatedFieldInjector.inject[ FDL, RVt, BuildMapVal, OtherSchema ]( translatedFieldDescriptions, fields, initMapVal )

        inline erasedValue[ AFt ] match {
            case _ : Nothing =>
                buildMapVal( buildMapWithFields )
            case _ =>
                val afInjector = summonInline[ Injector.Aux[ Map[ String, AFt ], BuildMapVal, OtherSchema[ AFt ], BuildMapVal ] ]
                val afTranslator = summonInline[ SchemaTranslator[ AFt, AFSt, OtherSchema ] ]
                val afSchema = afTranslator.translate( product.additionalFieldsSchema )
                val fieldsSet : Set[ String ] = ???
                val fixedAdditionalFields = additionalFields.filter( v => !fieldsSet.contains( v._1 ) )
                val buildMapWithAdditionalFields = afInjector.inject( fixedAdditionalFields, buildMapWithFields, afSchema)

                buildMapVal( buildMapWithAdditionalFields )
        }
    }

    type TFD = [ X ] =>> TranslatedFieldDescription[ X, OtherSchema ]

    inline def readerFromProduct[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt ](
        product : ProductShape[ T, Rt, RVt, AFt, AFSt ],
    )(
        using
        ftt : FieldTupleTranslator.Aux[ Rt, OtherSchema, FDL ],
        cx : CtxWrapTuplesConstraint[ TFD, FDL, RVt ],
        ex : FieldsExtractor[MapVal, FDL],
    ) : MapVal => T = { ( mapVal : MapVal ) =>
        val translatedFieldDescriptions = FieldTupleTranslator.translateFieldDescriptions[ Rt, OtherSchema ]( product.fieldDescriptions )
        val fieldValues = FieldsExtractor.extract[ MapVal, FDL ]( mapVal, translatedFieldDescriptions )
        
        inline fieldValues match {
            case correctFV : RVt =>
                inline erasedValue[ AFt ] match {
                    case _: Nothing => 
                        product.construct( correctFV, Map.empty[ String, AFt ] )
                    case _ =>
                        val afExtractor = summonInline[ SimpleExtractor.Aux[ MapVal, OtherSchema[ AFt ], Map[ String, AFt ] ] ]
                        val afTranslator = summonInline[ SchemaTranslator[ AFt, AFSt, OtherSchema ] ]
                        val afSchema = afTranslator.translate( product.additionalFieldsSchema )
                        val additionalFieldValues : Map[ String, AFt ] = afExtractor.extract( mapVal, afSchema )
                        product.construct( correctFV, additionalFieldValues )
                }
                
        }
        
    }

    inline given productTranslation[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt ](
        using
        ftt : FieldTupleTranslator.Aux[ Rt, OtherSchema, FDL ],
        cx : CtxWrapTuplesConstraint[ TFD, FDL, RVt ],
        ex : FieldsExtractor[MapVal, FDL],
    ) : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], OtherSchema ] = {
            new SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ], OtherSchema ] {
                def translate( schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, AFt, AFSt ] ] ) : OtherSchema[ T ] = {
                    val writer : T => MapVal = writerFromProduct[ T, Rt, RVt, FDL, AFt, AFSt ]( schema.shape )
                    val reader : MapVal => T = readerFromProduct[ T, Rt, RVt, FDL, AFt, AFSt ]( schema.shape )
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
