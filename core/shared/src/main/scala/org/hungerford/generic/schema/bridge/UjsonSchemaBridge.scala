//package org.hungerford.generic.schema.bridge
//
//import org.hungerford.generic.schema.product.ProductSchema
//import org.hungerford.generic.schema.product.field.{FieldDescriptionMapper, TranslatedFieldDescription}
//import org.hungerford.generic.schema.types.Extractor
//import org.hungerford.generic.schema.{NoSchema, Primitive}
//import shapeless._
//import shapeless.ops.hlist._
//import ujson.Value
//import upickle.default
//import upickle.default._
//
//import scala.language.higherKinds
//import scala.util.Try
//
//object UjsonSchemaBridge {
//
//
//    implicit def primitiveTranslation[ T, Rt ]( implicit rw : ReadWriter[ T ] ) : SchemaBridge[ T, Primitive, ReadWriter ] =
//        ( _ : Primitive[ T ] ) => rw
//
//
//    implicit def additionalFieldsExtractor[ AFt ](
//        implicit
//        sch : ReadWriter[ AFt ],
//    ) : AdditionalFieldsExtractor[ AFt, Map[ String, Value.Value ] ] = {
//        ( from : Map[ String, Value.Value ] ) => {
//            from
//              .filter( ( v : (String, Value) ) => Try( read[ AFt ]( v._2 )( sch ) ).toOption.nonEmpty )
//              .mapValues( ( v : Value ) => read[ AFt ]( v )( sch ) )
//        }
//    }
//
//    implicit def additionalFieldsWriter[ AFt ] : AdditionalFieldsWriter[ AFt, Map[ String, Value.Value ], ReadWriter ] = {
//        ( fields : Map[ String, AFt ], to : Map[ String, Value ], using : default.ReadWriter[ AFt ] ) => {
//            val newFields = fields.mapValues( v => upickle.default.writeJs( v )( using ) )
//            to ++ newFields
//        }
//    }
//
//    implicit def fieldWriter[ T ] : FieldWriter[ T, Value.Value, ReadWriter ] = {
//        ( field : T, to : Value, using : TranslatedFieldDescription[ T, default.ReadWriter ] ) => {
//            val originalFields : Map[String, Value ] = to.obj.toMap
//            val fieldValue : Value = upickle.default.writeJs( field )( using.schema )
//            val newFields : Map[ String, Value ] = originalFields + ((using.fieldName, fieldValue))
//            ujson.Obj.from( newFields )
//        }
//    }
//
//    // Need this to be an object, so that mapper can find its cases
//    object RWFieldDescriptionMapper extends FieldDescriptionMapper[ ReadWriter ]
//
//    // For no fields
//    implicit def productTranslation[ T, Rt <: HList, RVt <: HList, FDL <: HList, AFt, AFtRt ](
//        fm : Mapper[ RWFieldDescriptionMapper.type, Rt ] { type Out = FDL },
//        pfe : Extractor.Aux[ Value.Value, HNil, FDL, RVt ],
//        afe : AdditionalFieldsExtractor[ AFt, Value.Value ],
//        flw : FieldListWriter[ RVt, FDL, Map[ String, Value.Value ] ],
//        afw : AdditionalFieldsWriter[ AFt, Map[ String, Value.Value ], ReadWriter ],
//        aftSchema : ReadWriter[ AFt ]
//    ) : SchemaBridge[ T, ({ type A[X] = ProductSchema[ X, Rt,  RVt, AFt, AFtRt ] })#A, ReadWriter ] =
//        new SchemaBridge[ T, ({ type A[X] = ProductSchema[ X, Rt,  RVt, AFt, AFtRt ] })#A, ReadWriter ] {
//        override def translate(
//            schema : ProductSchema[ T, Rt,  RVt, AFt, AFtRt ],
//        ) : default.ReadWriter[ T ] = {
//                val fieldDescriptions : FDL = schema.fieldDescriptions.map( RWFieldDescriptionMapper )( fm )
//                readwriter[ Value.Value ].bimap[ T ](
//                    // generate ujson Value from object value
//                    ( value : T ) => {
//                        val (fields : RVt, additionalFields : Map[ String, AFt ]) = schema.deconstructor( value )
//                        val startingMap : Map[ String, Value.Value ] = Map.empty
//                        val updatedMap = flw.write( fields, startingMap, fieldDescriptions )
//                        val finalMap = afw.write( additionalFields, updatedMap, aftSchema )
//                        ujson.Obj.from( finalMap )
//                    },
//                    ( source : Value.Value ) => {
//                        val fieldValues = pfe.extract( source, HNil, fieldDescriptions )
//                        val additionalFieldValues = afe.extract( source )
//                        schema.constructor( fieldValues, additionalFieldValues )
//                    }
//                )
//        }
//    }
//
//}
//
//trait AdditionalFieldsExtractor[ AFt, Source ] {
//    def extract( from : Source ) : Map[ String, AFt ]
//}
//
//object AdditionalFieldsExtractor {
//    def apply[ AFt, Source ](
//        implicit afe : AdditionalFieldsExtractor[ AFt, Source ],
//    ) : AdditionalFieldsExtractor[ AFt, Source ] = afe
//}
//
//trait AdditionalFieldsWriter[ AFt, Target, OtherSchema[ _ ] ] {
//    def write( fields : Map[ String, AFt ], to : Target, using : OtherSchema[ AFt ] ) : Target
//}
//
//object AdditionalFieldsWriter {
//    def apply[ AFt, Target, OtherSchema[ _ ] ](
//        implicit afw : AdditionalFieldsWriter[ AFt, Target, OtherSchema ],
//    ) : AdditionalFieldsWriter[ AFt, Target, OtherSchema ] = afw
//}
//
//trait FieldWriter[ T, Target, OtherSchema[ _ ] ] {
//    def write( field : T, to : Target, using : TranslatedFieldDescription[ T, OtherSchema ] ) : Target
//}
//
//object FieldWriter {
//    def apply[ T, Target, OtherSchema[ _ ] ](
//        implicit fw : FieldWriter[ T, Target, OtherSchema ],
//    ) : FieldWriter[ T, Target, OtherSchema ] = fw
//}
//
//trait FieldListWriter[ RVt <: HList, FDL <: HList, Target ] {
//    def write( fields : RVt, to : Target, using : FDL ) : Target
//}
//
//object FieldListWriter {
//    def apply[ RVt <: HList, FDL <: HList, Target ](
//        implicit flw : FieldListWriter[ RVt, FDL, Target ],
//    ) : FieldListWriter[ RVt, FDL, Target ] = flw
//
//    implicit def hnilFLWriter[ Target ] : FieldListWriter[ HNil, HNil, Target ] = {
//        ( _ : HNil, to : Target, _ : HNil ) => to
//    }
//
//    implicit def genericFLWriter[ OtherSchema[ _ ], RVHead, RVTail <: HList, FDHead <: TranslatedFieldDescription[ RVHead, OtherSchema ], FDTail <: HList, Target ](
//        implicit
//        fw : FieldWriter[ RVHead, Target, OtherSchema ],
//        flw : FieldListWriter[ RVTail, FDTail, Target ],
//    ) : FieldListWriter[ RVHead :: RVTail, FDHead :: FDTail, Target ] = {
//        ( fields : RVHead :: RVTail, to : Target, using : FDHead :: FDTail ) => {
//            val newTarget = fw.write( fields.head, to, using.head )
//            flw.write( fields.tail, newTarget, using.tail )
//        }
//    }
//}
