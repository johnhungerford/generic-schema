package org.hungerford.generic.schema.coproduct.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, TypeName}
import org.hungerford.generic.schema.translation.{TypeCache, TypeCacheRetriever, RecursiveSchemaTranslator}

trait CoproductEncoderTranslation[ OtherSchema[ _ ], Sink ]
  extends WithSubtypeWriter[ OtherSchema, Sink ] {

    def buildCoproductEncoder[ T ](
        decode : T => Sink,
    ) : OtherSchema[ T ]

    trait CoproductWriter[ V, R, Cache <: TypeCache ] {
        def write( value : V, informedBy : R, cache : Cache ) : Option[ Sink ]
    }

    object CoproductWriter {
        given subtypeWriter[ T, ST, D, DN, DV, N <: TypeName, STS, Cache <: TypeCache ](
            using
            st : RecursiveSchemaTranslator[ ST, STS, Cache, OtherSchema ],
            sw : SubtypeWriter[ ST, N ],
        ) : CoproductWriter[ T, Subtype[ T, ST, D, DN, DV, N, STS ], Cache ] with {
            override def write(
                value: T,
                informedBy: Subtype[ T, ST, D, DN, DV, N, STS ],
                cache : Cache,
            ) : Option[ Sink ] = {
                informedBy.fromSuper( value ).map( stVal => {
                    val os = st.translate( informedBy.schema, cache )
                    sw.write( stVal, informedBy, os )
                } )
            }
        }

        given lazySubtypeWriter[ T, ST, D, DN, DV, N <: TypeName, Cache <: TypeCache ](
            using
            tr : TypeCacheRetriever.Aux[ Cache, ST, OtherSchema[ ST ] ],
            sw : SubtypeWriter[ ST, N ],
        ) : CoproductWriter[ T, LazySubtype[ T, ST, D, DN, DV, N ], Cache ] with {
            override def write(
                value: T,
                informedBy: LazySubtype[ T, ST, D, DN, DV, N ],
                cache : Cache,
            ) : Option[ Sink ] = {
                informedBy.fromSuper( value ).map( stVal => {
                    val encoder = tr.get( cache ).get()
                    sw.write( stVal, informedBy, encoder )
                } )
            }
        }

        given emptyWriter[ T, Cache <: TypeCache ] : CoproductWriter[ T, EmptyTuple, Cache ] with {
            def write( value: T, informedBy : EmptyTuple, cache : Cache ): Option[ Sink ] = None
        }

        given tupleWriter[ T, H, Tail <: Tuple, Cache <: TypeCache ](
            using
            h : CoproductWriter[ T, H, Cache ],
            t : CoproductWriter[ T, Tail, Cache ],
        ) : CoproductWriter[ T, H *: Tail, Cache ] with {
            override def write( value: T, informedBy: H *: Tail, cache : Cache ) : Option[ Sink ] = {
                h.write( value, informedBy.head, cache ) match {
                    case res@Some( _ ) => res
                    case _ => t.write( value, informedBy.tail, cache )
                }
            }
        }
    }

    given encoderTranslator[ T, R <: Tuple, RV <: Tuple, D, DN, Cache <: TypeCache ](
        using
        writer : CoproductWriter[ T, R, TypeCache.Cached[Cache, T, RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Cache, OtherSchema ]] ],
    ) : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Cache, OtherSchema ] with { self =>
        type NextCache = TypeCache.Cached[Cache, T, RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Cache, OtherSchema ]]

        override def translate(
            schema: Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            cache : Cache,
        ): OtherSchema[ T ] = {
            val nextCache : NextCache = cache.add[ T ]( self ).asInstanceOf[ NextCache ]
            buildCoproductEncoder[ T ]( value => writer.write( value, schema.shape.subtypeDescriptions, nextCache ).get )
        }
    }

}
