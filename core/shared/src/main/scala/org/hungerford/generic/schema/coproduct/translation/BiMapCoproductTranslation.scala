package org.hungerford.generic.schema.coproduct.translation

import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator}

trait BiMapCoproductTranslation[ RW[ _ ], DecoderSch[ _ ], EncoderSch[ _ ], Source, Sink ]
    extends CoproductDecoderTranslation[ DecoderSch, Source ]
      with CoproductEncoderTranslation[ EncoderSch, Sink ] {

    def buildCoproductSchema[ T ]( enc : EncoderSch[ T ], dec : DecoderSch[ T ] ) : RW[ T ]

    given bimapCoproductTrans[ T, R <: Tuple, RV <: Tuple, D, DN ](
        using
        encTr : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], EmptyTuple, EncoderSch ],
        decTr : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], EmptyTuple, DecoderSch ],
    ) : SchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], RW ] with {
        override def translate( schema: Aux[ T, CoproductShape[ T, R, RV, D, DN ] ] ): RW[ T ] =
            buildCoproductSchema(
                encTr.translate( schema, EmptyTuple ),
                decTr.translate( schema, EmptyTuple ),
            )
    }

}
