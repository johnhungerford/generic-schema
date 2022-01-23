package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.{ProductConstructor, ProductDeconstructor}
import org.hungerford.generic.schema.product.field.{Field, FieldName, FieldInjector}
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.types.{CtxWrapTuplesConstraint, Injector, SimpleExtractor}

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

    protected def extractField[ T, F ]( from : MapVal, field : Field[ T, F ], schema : OtherSchema[ F ] ) : F

    protected def extractAdditionalFields[ T ]( from : MapVal, informedBy : OtherSchema[ T ] ) : Map[ String, T ]

    protected def writeField[ T, F ]( value : F, to : BuildMapVal, field : Field[ T, F ], schema : OtherSchema[ F ] ) : BuildMapVal

    protected def writeAdditionalFields[ T ]( from : Map[ String, T ], to : BuildMapVal, informedBy : OtherSchema[ T ] ) : BuildMapVal

    trait BiMapInjector[ T, F ] {
        def inject( value : F, into : BuildMapVal, field : Field[ T, F ], schema : OtherSchema[ F ] ) : BuildMapVal
    }

    object BiMapInjector {
        given fromWriteField[ T, F ] : BiMapInjector[ T, F ] with {
            def inject( value : F, into : BuildMapVal, field : Field[ T, F ], schema : OtherSchema[ F ] ) : BuildMapVal = {
                writeField[ T, F ]( value, into, field, schema )
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

        given[ T, F, N <: FieldName, S, RVTail <: Tuple, RTail <: Tuple ] (
            using
            trans : SchemaTranslator[ F, S, OtherSchema ],
            headInjector : BiMapInjector[ T, F ],
            tailInjector : BiMapTupleInjector[ RVTail, RTail ],
        ) : BiMapTupleInjector[ F *: RVTail, Field.Aux[ T, F, N, S ] *: RTail ] with {
            def inject( value : F *: RVTail, into : BuildMapVal, informedBy : Field.Aux[ T, F, N, S ] *: RTail ) : BuildMapVal = {
                val headSchema = trans.translate( informedBy.head.schema )
                val headInjected = headInjector.inject( value.head, into, informedBy.head, headSchema )
                tailInjector.inject( value.tail, headInjected, informedBy.tail )
            }
        }
    }

    trait BiMapExtractor[ T, F ] {
        def extract( from : MapVal, field : Field[ T, F ], schema : OtherSchema[ F ]  ) : F
    }

    object BiMapExtractor {
        given[ T, F ] : BiMapExtractor[ T, F ] with {
            def extract( from : MapVal, field : Field[ T, F ], schema : OtherSchema[ F ] ) : F = {
                extractField[ T, F ]( from, field, schema )
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


        given[ T, F, N <: FieldName, S, Tail <: Tuple, TailRes <: Tuple ](
            using
            trans : SchemaTranslator[ F, S, OtherSchema ],
            headExtr : BiMapExtractor[ T, F ],
            tailExtr : BiMapTupleExtractor.Aux[ Tail, TailRes ],

        ) : BiMapTupleExtractor.Aux[ Field.Aux[ T, F, N, S ] *: Tail, F *: TailRes ] =
            new BiMapTupleExtractor[ Field.Aux[ T, F, N, S ] *: Tail ] {
                type Out = F *: TailRes

                def extract( from : MapVal, informedBy : Field.Aux[ T, F, N, S ] *: Tail ) : F *: tailExtr.Out = {
                    val headSchema = trans.translate( informedBy.head.schema )
                    headExtr.extract( from, informedBy.head, headSchema ) *: tailExtr.extract( from, informedBy.tail )
                }
            }
    }

    given fieldInjector[ T, F, N <: FieldName, S ](
        using
        sr : SchemaTranslator[ F, S, OtherSchema ],
    ) : FieldInjector[ T, F, N, S, BuildMapVal ] with {
        def inject( field : Field.Aux[ T, F, N, S ], value : F, into : BuildMapVal ) : BuildMapVal =
            writeField[ T, F ]( value, into, field, sr.translate( field.schema ) )
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


    given encoderWithoutAF[ T, Rt <: Tuple, RVt <: Tuple, C ] (
        using
        inj : BiMapTupleInjector[ RVt, Rt ],
        prodDeconstr : ProductDeconstructor.Aux[ T, Rt, RVt ]
    ) : Encoder[ T, ProductShape[ T, Rt, RVt, Nothing, Unit, Unit, C ], MapVal ] with {
        def encode( value : T, product : ProductShape[ T, Rt, RVt, Nothing, Unit, Unit, C ] ) : MapVal = {
            val fields = prodDeconstr.deconstruct( value, product.fieldDescriptions )
            val buildMapWithFields : BuildMapVal = inj.inject( fields, initMapVal, product.fieldDescriptions )
            buildMapVal( buildMapWithFields )
        }
    }

    given encoderWithAF[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt, AFEt, C ] (
        using
        inj : BiMapTupleInjector[ RVt, Rt ],
        afInjector : Injector.Aux[ Map[ String, AFt ], BuildMapVal, OtherSchema[ AFt ], BuildMapVal ],
        afTranslator : SchemaTranslator[ AFt, AFSt, OtherSchema ],
        prodDeconstr : ProductDeconstructor.Aux[ T, (AFEt, Rt), (Map[ String, AFt ], RVt) ],
    ) : Encoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, AFEt, C ], MapVal ] with {
        def encode( value : T, product : ProductShape[ T, Rt, RVt, AFt, AFSt, AFEt, C ] ) : MapVal = {
            val (additionalFields, fields) = prodDeconstr.deconstruct( value, (product.afExtractor, product.fieldDescriptions) )
            val additionalFieldsCorrected = additionalFields -- product.fieldNames
            val buildMapWithFields : BuildMapVal = inj.inject( fields, initMapVal, product.fieldDescriptions )
            val afSchema = afTranslator.translate( product.additionalFieldsSchema )
            val buildMapWithAF : BuildMapVal = afInjector.inject( additionalFieldsCorrected, buildMapWithFields, afSchema )
            buildMapVal( buildMapWithAF )
        }
    }

    given decoderWithAF[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt, AFEt, C ] (
        using
        ex : BiMapTupleExtractor.Aux[ Rt, RVt ],
        afExtractor : SimpleExtractor.Aux[ MapVal, OtherSchema[ AFt ], Map[ String, AFt ] ],
        afTranslator : SchemaTranslator[ AFt, AFSt, OtherSchema ],
        prodConstr : ProductConstructor[ C, RVt, AFt, T ],
    ) : Decoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, AFEt, C ], MapVal ] with {
        def decode( value : MapVal, product : ProductShape[ T, Rt, RVt, AFt, AFSt, AFEt, C ] ) : T = {
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

    given decoderWithoutAF[ T, Rt <: Tuple, RVt <: Tuple, C ] (
        using
        ex : BiMapTupleExtractor.Aux[ Rt, RVt ],
        prodConstr : ProductConstructor[ C, RVt, Nothing, T ],
    ) : Decoder[ T, ProductShape[ T, Rt, RVt, Nothing, Unit, Unit, C ], MapVal ] with {
        def decode( value : MapVal, product : ProductShape[ T, Rt, RVt, Nothing, Unit, Unit, C ] ) : T = {
            val fieldValues = ex.extract( value, product.fieldDescriptions )
            prodConstr.construct( product.constructor )( fieldValues )
        }
    }

    inline given bimapProductTranslation[ T, Rt <: Tuple, RVt <: Tuple, AFt, AFSt, AFEt, C ](
        using
        enc : Encoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, AFEt, C ], MapVal ],
        dec : Decoder[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, AFEt, C ], MapVal ],
    ) : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, AFEt, C ], OtherSchema ] = {
        new SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, AFEt, C ], OtherSchema ] {
            def translate( schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, AFEt, C ] ] ) : OtherSchema[ T ] = {
                val writer : T => MapVal = ( value : T ) => enc.encode( value, schema.shape )
                val reader : MapVal => T = ( value : MapVal ) => dec.decode( value, schema.shape )
                schemaFromBimap( writer, reader )
            }
        }
    }

    def translate[ T, Rt <: Tuple, RVt <: Tuple, FDL <: Tuple, AFt, AFSt, AFEt, C ](
        schema : Schema.Aux[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, AFEt, C ] ],
    )(
        using
        tr : SchemaTranslator[ T, ProductShape[ T, Rt, RVt, AFt, AFSt, AFEt, C ], OtherSchema ],
    ) : OtherSchema[ T ] = tr.translate( schema )

}
