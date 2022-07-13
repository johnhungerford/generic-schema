package org.hungerford.generic.schema.circe

import io.circe.{Codec, Decoder, Encoder}
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator}

import scala.compiletime.{erasedValue, error}

trait CirceSchemaTranslation
  extends CirceProductSchemaTranslation
    with CirceCoproductSchemaTranslation
    with CirceSingletonSchemaTranslation {

    given [ T ](
        using
        enc : Encoder[ T ],
        dec : Decoder[ T ],
    ) : Codec[ T ] = Codec.from( dec, enc )

    given [ T, S, Trans <: Tuple ](
        using
        encTr: RecursiveSchemaTranslator[ T, S, EmptyTuple, Encoder ],
        decTr: RecursiveSchemaTranslator[ T, S, EmptyTuple, Decoder ],
    ): RecursiveSchemaTranslator[ T, S, Trans, Codec ] with {
        override def translate(
            schema: Aux[ T, S ],
            trans: Trans,
        ): Codec[ T ] = {
            val encoder = encTr.translate( schema, EmptyTuple )
            val decoder = decTr.translate( schema, EmptyTuple )
            Codec.from( decoder, encoder )
        }
    }
}

object CirceSchemaTranslation
  extends CirceSchemaTranslation

trait TransRetriever[ Trans <: Tuple, T, OtherSchema[ _ ] ] {
    type S

    def getTranslator( from : Trans ): SchemaTranslator[ T, S, OtherSchema ]
}

trait TransRetrievers1 {
    given next[ NotTr, Next <: Tuple, T, St, OtherSchema[ _ ] ](
        using
        nextTrRt: TransRetriever.Aux[ Next, T, St, OtherSchema ],
    ): TransRetriever[ NotTr *: Next, T, OtherSchema ] with {
        type S = St

        def getTranslator( from: NotTr *: Next ): SchemaTranslator[ T, St, OtherSchema ] =
            nextTrRt.getTranslator( from.tail )
    }
}

object TransRetriever extends TransRetrievers1 {
    type Aux[ Trans <: Tuple, T, St, OtherSchema[ _ ] ] = TransRetriever[ Trans, T, OtherSchema ] { type S = St }

    inline def transConverter[ Trans <: Tuple, PriorTrans <: Tuple ] : Trans => PriorTrans = {
        inline erasedValue[ Trans ] match {
            case _ : PriorTrans => ( trans: Trans ) => trans.asInstanceOf[ PriorTrans ]
            case _ : (transHead *: PriorTrans) =>
                type TransHead = transHead
                ( trans: Trans ) => trans.asInstanceOf[ TransHead *: PriorTrans ].tail
            case _ : (transHead *: transTail) =>
                type TransHead = transHead
                type TransTail = transTail
                val nextConverter = transConverter[ TransTail, PriorTrans ]
                ( trans: Trans ) => nextConverter( trans.asInstanceOf[ TransHead *: TransTail ].tail )
            case _ => error( "Unable to find prior translators in translators" )
        }
    }

    inline given isHead[ T, St, OtherSchema[ _ ], Next <: Tuple, PriorTrans <: Tuple ]: TransRetriever.Aux[ RecursiveSchemaTranslator[ T, St, PriorTrans, OtherSchema ] *: Next, T, St, OtherSchema ] = {
        type Trans = RecursiveSchemaTranslator[ T, St, PriorTrans, OtherSchema ] *: Next
        val tc = transConverter[ Trans, PriorTrans ]

        new TransRetriever[ RecursiveSchemaTranslator[ T, St, PriorTrans, OtherSchema ] *: Next, T, OtherSchema ] {
            type S = St

            def getTranslator(from: Trans ): SchemaTranslator[ T, S, OtherSchema ] = {
                val translator: RecursiveSchemaTranslator[ T, St, PriorTrans, OtherSchema ] = from.head
                new SchemaTranslator[ T, S, OtherSchema ] {
                    def translate( schema: Schema.Aux[ T, S ] ): OtherSchema[ T ] =
                        translator.translate( schema, tc( from ) )
                }
            }
        }
    }
}
