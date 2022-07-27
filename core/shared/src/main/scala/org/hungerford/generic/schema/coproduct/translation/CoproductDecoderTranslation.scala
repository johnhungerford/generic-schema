package org.hungerford.generic.schema.coproduct.translation

import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.{Schema, SchemaProvider}
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, TypeName}
import org.hungerford.generic.schema.product.field.FieldName
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator, TypeCache, TypeCacheRetriever}

trait CoproductDecoderTranslation[ OtherSchema[ _ ], Source ]
  extends WithSubtypeReader[ OtherSchema, Source ] {

    def buildCoproductDecoder[ T ](
        decode : Source => Option[ T ],
    ) : OtherSchema[ T ]

    trait CoproductReader[ T, R, D, DN, Cache <: TypeCache ] {
        def read( from : Source, subtypes : R, cache : Cache ) : Option[ T ]
    }

    object CoproductReader {
        given subtypeReaderWithoutDiscriminator[ T, ST, N <: TypeName, S, Cache <: TypeCache ](
            using
            st : RecursiveSchemaTranslator[ ST, S, Cache, OtherSchema ],
            str : SubtypeReader[ ST, N ],
        ) : CoproductReader[ T, Subtype[ T, ST, Unit, Unit, Unit, N, S ], Unit, Unit, Cache ] with {
            override def read(
                from: Source,
                subtypes: Subtype[ T, ST, Unit, Unit, Unit, N, S ],
                cache: Cache,
            ) : Option[ T ] = {
                val decoder = st.translate( subtypes.schema, cache )
                str
                  .read( from, subtypes, decoder )
                  .flatMap { v =>
                      if ( subtypes.validators.forall( _.isValid( v ) ) ) Some( subtypes.toSuper( v ) )
                      else None
                  }
            }
        }

        given lazySubtypeReaderWithoutDiscriminator[ T, ST, N <: TypeName, Cache <: TypeCache ](
            using
            tr: TypeCacheRetriever.Aux[ Cache, ST, OtherSchema[ ST ] ],
            str : SubtypeReader[ ST, N ],
        ) : CoproductReader[ T, LazySubtype[ T, ST, Unit, Unit, Unit, N ], Unit, Unit, Cache ] with {
            override def read(
                from: Source,
                subtypes: LazySubtype[ T, ST, Unit, Unit, Unit, N ],
                cache: Cache,
            ) : Option[ T ] = {
                val decoder = tr.get( cache ).get()
                str
                  .read( from, subtypes, decoder )
                  .flatMap { v =>
                      if ( subtypes.validators.forall( _.isValid( v ) ) ) Some( subtypes.toSuper( v ) )
                      else None
                  }
            }
        }

        given subtypeReaderWithDiscriminator[ T, ST, N <: TypeName, S, D, DN <: FieldName, DV <: D & Singleton, DS, Cache <: TypeCache ](
            using
            dsp : SchemaProvider.Aux[ D, DS ],
            dst : SchemaTranslator[ D, DS, OtherSchema ],
            st : RecursiveSchemaTranslator[ ST, S, Cache, OtherSchema ],
            str : SubtypeReader[ ST, N ],
            vo : ValueOf[ DN ],
            dr : DiscrReader[ D ],
        ) : CoproductReader[ T, Subtype[ T, ST, D, DN, DV, N, S ], D, DN, Cache ] = {
            val dSch = dst.translate( dsp.provide )

            new CoproductReader[ T, Subtype[ T, ST, D, DN, DV, N, S ], D, DN, Cache ] {
                override def read(
                    from: Source,
                    subtypes: Subtype[ T, ST, D, DN, DV, N, S ],
                    cache: Cache,
                ): Option[ T ] = {
                    val discrName : String = subtypes.discriminatorName
                    val discrValue : D = subtypes.discriminatorValue

                    dr.read( from, discrName, dSch ) match {
                        case None => throw IllegalStateException( "" )
                        case Some( `discrValue` ) =>
                            val decoder = st.translate( subtypes.schema, cache )
                            str
                              .read( from, subtypes, decoder )
                              .flatMap { v =>
                                  if ( subtypes.validators.forall( _.isValid( v ) ) )
                                      Some( subtypes.toSuper( v ) )
                                  else None
                              }
                        case _ => None
                    }
                }
            }
        }

        given emptyReader[ T, D, DN, Cache <: TypeCache ] : CoproductReader[ T, EmptyTuple, D, DN, Cache ] with {
            override def read(
                from: Source, subtypes: EmptyTuple, cache: Cache,
            ) : Option[ T ] = None
        }

        given tupleReader[ T, D, DN, H, Tail <: Tuple, Cache <: TypeCache ](
            using
            h : CoproductReader[ T, H, D, DN, Cache ],
            t : CoproductReader[ T, Tail, D, DN, Cache ],
        ) : CoproductReader[ T, H *: Tail, D, DN, Cache ] with {
            override def read(
                from: Source, subtypes: H *: Tail, cache: Cache,
            ) : Option[ T ] = {
                h.read( from, subtypes.head, cache ) match {
                    case res@Some( v ) =>
                        res
                    case _ =>
                        t.read( from, subtypes.tail, cache )
                }
            }
        }
    }

    given decoderTranslator[ T, R <: Tuple, RV <: Tuple, D, DN, Cache <: TypeCache ](
        using
        reader : CoproductReader[ T, R, D, DN, TypeCache.Cached[Cache, T, OtherSchema[T]] ],
    ) : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Cache, OtherSchema ] with {
        override def translate(
            schema: Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            cache : Cache,
        ): OtherSchema[ T ] = {
            val nextCache = cache.add[ T ]( translate(schema, cache ) ).asInstanceOf[ TypeCache.Cached[Cache, T, OtherSchema[T]] ]
            buildCoproductDecoder[ T ]( source => reader.read( source, schema.shape.subtypeDescriptions, nextCache ) )
        }
    }

}
