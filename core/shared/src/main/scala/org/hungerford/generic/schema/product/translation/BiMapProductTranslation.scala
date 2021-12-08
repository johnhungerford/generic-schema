package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.{ProductShape, CtxWrapTuplesConstraint}
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.product.field.{FieldName, Field, FieldTranslator, TranslatedFieldDescription, TranslatedFieldInjector}
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.types.{Injector, SimpleExtractor}

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
        given[ T ] : BiMapInjector[ T ] with {
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

        given[ T, N <: FieldName, S, RVTail <: Tuple, RTail <: Tuple ] (
            using
            trans : FieldTranslator[ T, N, S, OtherSchema ],
            headInjector : BiMapInjector[ T ],
            tailInjector : BiMapTupleInjector[ RVTail, RTail ],
        ) : BiMapTupleInjector[ T *: RVTail, Field.Aux[ T, N, S ] *: RTail ] with {
            def inject( value : T *: RVTail, into : BuildMapVal, informedBy : Field.Aux[ T, N, S ] *: RTail ) : BuildMapVal = {
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
        given[ T ] : BiMapExtractor[ T ] with {
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
        type Aux[ R <: Tuple, O <: Tuple ] = BiMapTupleExtractor[ R ] {type Out = O}

        given BiMapTupleExtractor[ EmptyTuple ] with {
            type Out = EmptyTuple

            def extract( from : MapVal, informedBy : EmptyTuple ) : EmptyTuple = EmptyTuple
        }


        given[ T, N <: FieldName, S, Tail <: Tuple, TailRes <: Tuple ](
            using
            trans : FieldTranslator[ T, N, S, OtherSchema ],
            headExtr : BiMapExtractor[ T ],
            tailExtr : BiMapTupleExtractor.Aux[ Tail, TailRes ],

        ) : BiMapTupleExtractor.Aux[ Field.Aux[ T, N, S ] *: Tail, T *: TailRes ] =
            new BiMapTupleExtractor[ Field.Aux[ T, N, S ] *: Tail ] {
                type Out = T *: TailRes

                def extract( from : MapVal, informedBy : Field.Aux[ T, N, S ] *: Tail ) : T *: tailExtr.Out = {
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


    type Ctx = [ X ] =>> TranslatedFieldDescription[ X, OtherSchema ]

    given encoderWithoutAF[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, C, DC ] (
        using
        ctx : CtxWrapTuplesConstraint[ Ctx, FDL, RVt ],
        inj : BiMapTupleInjector[ RVt, Rt ],
        prodDeconstr : ProductDeconstructor[ DC, RVt, Nothing, T ]
    ) : Encoder[ T, ProductShape[ T, Rt, RVt, Nothing, Unit, C, DC ], MapVal ] with {
        def encode( value : T, product : ProductShape[ T, Rt, RVt, Nothing, Unit, C, DC ] ) : MapVal = {
            val (fields, _) = prodDeconstr.deconstruct( product.deconstructor )( value )
            val buildMapWithFields : BuildMapVal = inj.inject( fields, initMapVal, product.fieldDescriptions )
            buildMapVal( buildMapWithFields )
        }
    }

    given encoderWithAF[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt, C, DC ] (
        using
        ctx : CtxWrapTuplesConstraint[ Ctx, FDL, RVt ],
        inj : BiMapTupleInjector[ RVt, Rt ],
        afInjector : Injector.Aux[ Map[ String, AFt ], BuildMapVal, OtherSchema[ AFt ], BuildMapVal ],
        afTranslator : SchemaTranslator[ AFt, AFSt, OtherSchema ],
        prodDeconstr : ProductDeconstructor[ DC, RVt, AFt, T ],
    ) : Encoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, C, DC ], MapVal ] with {
        def encode( value : T, product : ProductShape[ T, Rt, RVt, AFt, AFSt, C, DC ] ) : MapVal = {
            val (fields, additionalFields) = prodDeconstr.deconstruct( product.deconstructor )( value )
            val additionalFieldsCorrected = additionalFields -- product.fieldNames
            val buildMapWithFields : BuildMapVal = inj.inject( fields, initMapVal, product.fieldDescriptions )
            val afSchema = afTranslator.translate( product.additionalFieldsSchema )
            val buildMapWithAF : BuildMapVal = afInjector.inject( additionalFieldsCorrected, buildMapWithFields, afSchema )
            buildMapVal( buildMapWithAF )
        }
    }

    given decoderWithAF[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt, C, DC ] (
        using
        ex : BiMapTupleExtractor.Aux[ Rt, RVt ],
        afExtractor : SimpleExtractor.Aux[ MapVal, OtherSchema[ AFt ], Map[ String, AFt ] ],
        afTranslator : SchemaTranslator[ AFt, AFSt, OtherSchema ],
        prodConstr : ProductConstructor[ C, RVt, AFt, T ],
    ) : Decoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, C, DC ], MapVal ] with {
        def decode( value : MapVal, product : ProductShape[ T, Rt, RVt, AFt, AFSt, C, DC ] ) : T = {
            val fieldValues = ex.extract( value, product.fieldDescriptions )
            val afSchema = afTranslator.translate( product.additionalFieldsSchema )
            val additionalFieldValues = afExtractor.extract( value, afSchema )
            prodConstr.construct(
                product.constructor,
            )(
                fieldValues,
                additionalFieldValues,
            )
        }
    }

    given decoderWithoutAF[ T, Rt <: Tuple, RVt <: Tuple, C, DC ] (
        using
        ex : BiMapTupleExtractor.Aux[ Rt, RVt ],
        prodConstr : ProductConstructor[ C, RVt, Nothing, T ],
    ) : Decoder[ T, ProductShape[ T, Rt, RVt, Nothing, Unit, C, DC ], MapVal ] with {
        def decode( value : MapVal, product : ProductShape[ T, Rt, RVt, Nothing, Unit, C, DC ] ) : T = {
            val fieldValues = ex.extract( value, product.fieldDescriptions )
            prodConstr.construct( product.constructor )( fieldValues )
        }
    }

    inline given bimapProductTranslation[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt, C, DC ](
        using
        enc : Encoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, C, DC ], MapVal ],
        dec : Decoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, C, DC ], MapVal ],
    ) : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, C, DC ], OtherSchema ] = {
        new SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, C, DC ], OtherSchema ] {
            def translate( schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, C, DC ] ] ) : OtherSchema[ T ] = {
                val writer : T => MapVal = ( value : T ) => enc.encode( value, schema.shape )
                val reader : MapVal => T = ( value : MapVal ) => dec.decode( value, schema.shape )
                schemaFromBimap( writer, reader )
            }
        }
    }

    def translate[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt, C, DC ](
        schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, C, DC ] ],
    )(
        using
        tr : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, C, DC ], OtherSchema ],
    ) : OtherSchema[ T ] = tr.translate( schema )

}
