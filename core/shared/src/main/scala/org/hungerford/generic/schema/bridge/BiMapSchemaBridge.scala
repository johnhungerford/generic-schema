package org.hungerford.generic.schema.bridge

import org.hungerford.generic.schema.product.ProductSchema
import org.hungerford.generic.schema.product.field.{FieldDescriptionMapper, TranslatedFieldDescription}
import org.hungerford.generic.schema.types.{Extractor, Injector, SimpleExtractor}
import org.hungerford.generic.schema.{NoSchema, Primitive, Schema}
import shapeless._
import shapeless.ops.hlist._
import ujson.Value
import upickle.default
import upickle.default._

import scala.language.higherKinds
import scala.util.Try

trait BiMapProductSchemaBridge[ OtherSchema[ _ ], MapVal, BuildMapVal ] {

    /**
     * Construct a schema from the two parts of a bimap.
     * @param to T => MapVal : writer
     * @param from MapVal => T : reader
     * @tparam T type being read/written
     * @return type class instance for some reader/writer
     */
    def schemaFromBimap[ T ]( to : T => MapVal, from : MapVal => T ) : OtherSchema[ T ]

    /**
     * Initial empty value for the type being mapped to and from, that can be built
     * by adding field values. For instance, if the value type is Map[ String, T ],
     * initMapVal would be Map.empty[ String, T ]
     * @return initial value of buildable bimap type
     */
    def initMapVal : BuildMapVal

    /**
     * Construct the final type to be bimapped to and from
     * @param buildableValue
     * @return
     */
    def buildMapVal( buildableValue : BuildMapVal ) : MapVal

    /**
     * Resolves type class instances for primitive schemas
     */
    implicit def primitiveTranslation[ T, Rt ]( implicit os : OtherSchema[ T ] ) : SchemaTranslation[ T, Primitive, OtherSchema ] =
        ( _ : Primitive[ T ] ) => os


    // Need this to be an object, so that mapper can find its cases
    object RWFieldDescriptionMapper extends FieldDescriptionMapper[ OtherSchema ]

    object Implicits {

        implicit def productTranslationWithoutAF[ T, Rt <: HList, RVt <: HList, FDL <: HList ](
            implicit
            fm : Mapper[ RWFieldDescriptionMapper.type, Rt ] { type Out = FDL },
            pfe : Extractor.Aux[ MapVal, HNil, FDL, RVt ],
            pfw : Injector.Aux[ RVt, BuildMapVal, FDL, BuildMapVal ],
        ) : SchemaTranslation[ T, ({ type A[X] = ProductSchema[ X, Rt,  RVt, Nothing, Nothing ] })#A, OtherSchema ] =
            new SchemaTranslation[ T, ({ type A[X] = ProductSchema[ X, Rt,  RVt, Nothing, Nothing ] })#A, OtherSchema ] {
                override def translate( schema : ProductSchema[ T, Rt, RVt, Nothing, Nothing ] ) : OtherSchema[ T ] = {
                    val fieldDescriptions : FDL = schema.fieldDescriptions.map( RWFieldDescriptionMapper )( fm )

                    val writer : T => MapVal = { ( value : T ) =>
                        val (fields : RVt, additionalFields : Map[ String, Nothing ]) = schema.deconstructor( value )
                        val buildMapWithFields = pfw.inject( fields, initMapVal, fieldDescriptions )
                        buildMapVal( buildMapWithFields )
                    }

                    val reader : MapVal => T = { ( mapVal : MapVal ) =>
                        val fieldValues = pfe.extract( mapVal, HNil, fieldDescriptions )
                        schema.constructor( fieldValues, Map.empty )
                    }

                    schemaFromBimap( writer, reader )
                }
            }

        implicit def productTranslationWithAF[ T, Rt <: HList, RVt <: HList, FDL <: HList, AFt, AFtRt ](
            fm : Mapper[ RWFieldDescriptionMapper.type, Rt ] { type Out = FDL },
            pfe : Extractor.Aux[ MapVal, HNil, FDL, RVt ],
            pfw : Injector.Aux[ RVt, BuildMapVal, FDL, BuildMapVal ],
            afe : SimpleExtractor.Aux[ MapVal, OtherSchema[ AFt ], Map[ String, AFt ] ],
            afw : Injector.Aux[ Map[ String, AFt ], BuildMapVal, OtherSchema[ AFt ], BuildMapVal ],
            afTrans : SchemaTranslation[ AFt, ({ type A[X] = Schema.Aux[ X, AFtRt ] })#A, OtherSchema ],
        ) : SchemaTranslation[ T, ({ type A[X] = ProductSchema[ X, Rt,  RVt, AFt, AFtRt ] })#A, OtherSchema ] =
            new SchemaTranslation[ T, ({ type A[X] = ProductSchema[ X, Rt,  RVt, AFt, AFtRt ] })#A, OtherSchema ] {

                override def translate(
                    schema : ProductSchema[ T, Rt,  RVt, AFt, AFtRt ],
                ) : OtherSchema[ T ] = {
                    val fieldDescriptions : FDL = schema.fieldDescriptions.map( RWFieldDescriptionMapper )( fm )
                    val aftSchema = afTrans.translate( schema.additionalFieldsSchema )

                    val writer : T => MapVal = { ( value : T ) =>
                        val (fields : RVt, additionalFields : Map[ String, AFt ]) = schema.deconstructor( value )
                        val buildMapWithAdditionalFields = afw.inject( additionalFields, initMapVal, aftSchema )
                        val buildMapWithFields = pfw.inject( fields, buildMapWithAdditionalFields, fieldDescriptions )
                        buildMapVal( buildMapWithFields )
                    }

                    val reader : MapVal => T = { ( mapVal : MapVal ) =>
                        val additionalFieldValues : Map[ String, AFt ] = afe.extract( mapVal, aftSchema )
                        val fieldValues = pfe.extract( mapVal, HNil, fieldDescriptions )
                        schema.constructor( fieldValues, additionalFieldValues )
                    }

                    schemaFromBimap( writer, reader )
                }

            }

        def rw[ T, Rt <: HList, RVt <: HList, FDL <: HList, AFt, AFtRt ](
            implicit
            schema : ProductSchema[ T, Rt,  RVt, AFt, AFtRt ],
            trans : SchemaTranslation[ T, ({ type A[X] = ProductSchema[ X, Rt,  RVt, AFt, AFtRt ] })#A, OtherSchema ],
        ) : OtherSchema[ T ] = trans.translate( schema )
    }

}
