package org.hungerford.generic.schema.coproduct.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, TypeName}
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaCacheRetriever}

trait CoproductEncoderTranslation[ OtherSchema[ _ ], Sink ]
  extends WithSubtypeWriter[ OtherSchema, Sink ] {

    def buildCoproductEncoder[ T ](
        decode : T => Sink,
    ) : OtherSchema[ T ]

    trait CoproductWriter[ V, R, Trans <: Tuple ] {
        def write( value : V, informedBy : R, trans : Trans ) : Option[ Sink ]
    }

    object CoproductWriter {
        given subtypeWriter[ T, ST, D, DN, DV, N <: TypeName, STS, Trans <: Tuple ](
            using
            st : RecursiveSchemaTranslator[ ST, STS, Trans, OtherSchema ],
            sw : SubtypeWriter[ ST, N ],
        ) : CoproductWriter[ T, Subtype[ T, ST, D, DN, DV, N, STS ], Trans ] with {
            override def write(
                value: T,
                informedBy: Subtype[ T, ST, D, DN, DV, N, STS ],
                trans: Trans,
            ) : Option[ Sink ] = {
                informedBy.fromSuper( value ).map( stVal => {
                    val os = st.translate( informedBy.schema, trans )
                    sw.write( stVal, informedBy, os )
                } )
            }
        }

        given lazySubtypeWriter[ T, ST, D, DN, DV, N <: TypeName, Trans <: Tuple ](
            using
            tr : SchemaCacheRetriever.Aux[ Trans, ST, OtherSchema[ ST ] ],
            sw : SubtypeWriter[ ST, N ],
        ) : CoproductWriter[ T, LazySubtype[ T, ST, D, DN, DV, N ], Trans ] with {
            override def write(
                value: T,
                informedBy: LazySubtype[ T, ST, D, DN, DV, N ],
                trans: Trans,
            ) : Option[ Sink ] = {
                informedBy.fromSuper( value ).map( stVal => {
                    val encoder = tr.getter( trans ).get()
                    sw.write( stVal, informedBy, encoder )
                } )
            }
        }

        given emptyWriter[ T, Trans <: Tuple ] : CoproductWriter[ T, EmptyTuple, Trans ] with {
            def write( value: T, informedBy : EmptyTuple, trans: Trans ): Option[ Sink ] = None
        }

        given tupleWriter[ T, H, Tail <: Tuple, Trans <: Tuple ](
            using
            h : CoproductWriter[ T, H, Trans ],
            t : CoproductWriter[ T, Tail, Trans ],
        ) : CoproductWriter[ T, H *: Tail, Trans ] with {
            override def write( value: T, informedBy: H *: Tail, trans: Trans ) : Option[ Sink ] = {
                h.write( value, informedBy.head, trans ) match {
                    case res@Some( _ ) => res
                    case _ => t.write( value, informedBy.tail, trans )
                }
            }
        }
    }

    given encoderTranslator[ T, R <: Tuple, RV <: Tuple, D, DN, Trans <: Tuple ](
        using
        writer : CoproductWriter[ T, R, RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, OtherSchema ] *: Trans ],
    ) : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, OtherSchema ] with { self =>
        override def translate(
            schema: Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            trans : Trans,
        ): OtherSchema[ T ] = {
            val nextTrans : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, OtherSchema ] *: Trans = self *: trans
            buildCoproductEncoder[ T ]( value => writer.write( value, schema.shape.subtypeDescriptions, nextTrans ).get )
        }
    }

}
