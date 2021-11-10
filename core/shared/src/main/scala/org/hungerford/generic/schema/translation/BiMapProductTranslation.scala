//package org.hungerford.generic.schema.translation
//
//import org.hungerford.generic.schema.product.ProductShape
//import org.hungerford.generic.schema.product.field.{FieldDescriptionMapper, TranslatedFieldDescription}
//import org.hungerford.generic.schema.types.{Extractor, Injector, SimpleExtractor}
//import org.hungerford.generic.schema.{NoSchema, Primitive, Schema}
//import shapeless._
//import shapeless.ops.hlist._
//
//import scala.language.higherKinds
//
//trait BiMapProductTranslation[ OtherSchema[ _ ], MapVal, BuildMapVal ] {
//
//    /**
//     * Construct a schema from the two parts of a bimap.
//     *
//     * @param to   T => MapVal : writer
//     * @param from MapVal => T : reader
//     * @tparam T type being read/written
//     * @return type class instance for some reader/writer
//     */
//    protected def schemaFromBimap[ T ]( to : T => MapVal, from : MapVal => T ) : OtherSchema[ T ]
//
//    /**
//     * Initial empty value for the type being mapped to and from, that can be built
//     * by adding field values. For instance, if the value type is Map[ String, T ],
//     * initMapVal would be Map.empty[ String, T ]
//     *
//     * @return initial value of buildable bimap type
//     */
//    protected def initMapVal : BuildMapVal
//
//    /**
//     * Construct the final type to be bimapped to and from
//     *
//     * @param buildableValue
//     * @return
//     */
//    protected def buildMapVal( buildableValue : BuildMapVal ) : MapVal
//
//    protected def extractField[ T ]( from : MapVal, using : TranslatedFieldDescription[ T, OtherSchema ] ) : T
//
//    protected def extractAdditionalFields[ T ]( from : MapVal, using : OtherSchema[ T ] ) : Map[ String, T ]
//
//    protected def writeField[ T ]( value : T, to : BuildMapVal, using : TranslatedFieldDescription[ T, OtherSchema ] ) : BuildMapVal
//
//    protected def writeAdditionalFields[ T ]( from : Map[ String, T ], to : BuildMapVal, using : OtherSchema[ T ] ) : BuildMapVal
//
//
//    // Need this to be an object, so that mapper can find its cases
//    object RWFieldDescriptionMapper extends FieldDescriptionMapper[ OtherSchema ]
//
//    implicit def fieldExtractor[ T, Rt ] : SimpleExtractor.Aux[ MapVal, TranslatedFieldDescription[ T, OtherSchema ], T ] = new SimpleExtractor[ MapVal,
//      TranslatedFieldDescription[ T, OtherSchema ] ] {
//        override type Out = T
//
//        override def extract( from : MapVal, using : TranslatedFieldDescription[ T, OtherSchema ] ) : Out = {
//            extractField( from, using )
//        }
//    }
//
//    implicit def fieldInjector[ T ] : Injector.Aux[ T, BuildMapVal, TranslatedFieldDescription[ T, OtherSchema ], BuildMapVal ] =
//        Injector.simpleInjector[ T, BuildMapVal, TranslatedFieldDescription[ T, OtherSchema ] ] {
//            ( value : T, target : BuildMapVal, using : TranslatedFieldDescription[ T, OtherSchema ] ) => {
//                writeField( value, target, using )
//            }
//        }
//
//    implicit def additionalFieldsExtractor[ T ] : SimpleExtractor.Aux[ MapVal, OtherSchema[ T ], Map[ String, T ] ] = {
//        new SimpleExtractor[ MapVal, OtherSchema[ T ] ] {
//            override type Out = Map[ String, T ]
//
//            override def extract( from : MapVal, using : OtherSchema[ T ] ) : Out = extractAdditionalFields( from, using )
//        }
//    }
//
//    implicit def additionalFieldsInjector[ T ] : Injector.Aux[ Map[ String, T ], BuildMapVal, OtherSchema[ T ], BuildMapVal ] =
//        Injector.simpleInjector[ Map[ String, T ], BuildMapVal, OtherSchema[ T ] ] {
//            ( value : Map[ String, T ], target : BuildMapVal, using : OtherSchema[ T ] ) =>
//                writeAdditionalFields( value, target, using )
//        }
//
//    implicit def productTranslationWithoutAF[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, Tup ](
//        implicit
//        fm : Mapper[ RWFieldDescriptionMapper.type, Rt ] {type Out = FDL},
//        pfe : Extractor.Aux[ MapVal, EmptyTuple, FDL, RVt ],
//        pfw : Injector.Aux[ RVt, BuildMapVal, FDL, BuildMapVal ],
//    ) : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, Nothing, Unit, Tup ], OtherSchema ] =
//        ( schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, Nothing, Unit, Tup ] ] ) => {
//            val fieldDescriptions : FDL = schema.shape.fieldDescriptions.map( RWFieldDescriptionMapper )( fm )
//
//            val writer : T => MapVal = { ( value : T ) =>
//                val (fields, _ : Map[ String, Nothing ]) = schema.shape.deconstructor( value )
//                val buildMapWithFields = pfw.inject( fields, initMapVal, fieldDescriptions )
//                buildMapVal( buildMapWithFields )
//            }
//
//            val reader : MapVal => T = { ( mapVal : MapVal ) =>
//                val fieldValues = pfe.extract( mapVal, EmptyTuple, fieldDescriptions )
//                schema.shape.constructor( fieldValues, Map.empty )
//            }
//
//            schemaFromBimap( writer, reader )
//        }
//
//    implicit def productTranslationWithAF[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt, Tup ](
//        implicit
//        fm : Mapper[ RWFieldDescriptionMapper.type, Rt ] {type Out = FDL},
//        pfe : Extractor.Aux[ MapVal, EmptyTuple, FDL, RVt ],
//        pfw : Injector.Aux[ RVt, BuildMapVal, FDL, BuildMapVal ],
//        afe : SimpleExtractor.Aux[ MapVal, OtherSchema[ AFt ], Map[ String, AFt ] ],
//        afw : Injector.Aux[ Map[ String, AFt ], BuildMapVal, OtherSchema[ AFt ], BuildMapVal ],
//        afTrans : SchemaTranslator[ AFt, AFSt, OtherSchema ],
//    ) : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, Tup ], OtherSchema ] =
//        ( schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, Tup ] ] ) => {
//            val fieldDescriptions : FDL = schema.shape.fieldDescriptions.map( RWFieldDescriptionMapper )( fm )
//            val aftSchema = afTrans.translate( schema.shape.additionalFieldsSchema )
//
//            // Note order of building might matter
//            val writer : T => MapVal = { ( value : T ) =>
//                val (fields, additionalFields : Map[ String, AFt ]) = schema.shape.deconstructor( value )
//                val fieldsSet = schema.shape.fields
//                val buildMapWithFields = pfw.inject( fields, initMapVal, fieldDescriptions )
//                val fixedAdditionalFields = additionalFields.filter( v => !fieldsSet.contains( v._1 ) )
//                val buildMapWithAdditionalFields = afw.inject( fixedAdditionalFields, buildMapWithFields, aftSchema )
//                buildMapVal( buildMapWithAdditionalFields )
//            }
//
//            val reader : MapVal => T = { ( mapVal : MapVal ) =>
//                val additionalFieldValues : Map[ String, AFt ] = afe.extract( mapVal, aftSchema )
//                val fieldValues = pfe.extract( mapVal, EmptyTuple, fieldDescriptions )
//                schema.shape.constructor( fieldValues, additionalFieldValues )
//            }
//
//            schemaFromBimap( writer, reader )
//        }
//
//}
