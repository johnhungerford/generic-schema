package org.hungerford.generic.schema.coproduct.translation

import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.{Schema, SchemaProvider}
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, TypeName}
import org.hungerford.generic.schema.product.field.FieldName
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator, TransRetriever}

trait CoproductDecoderTranslation[ OtherSchema[ _ ], Source ]
  extends WithSubtypeReader[ OtherSchema, Source ] {

    def buildCoproductDecoder[ T ](
        decode : Source => Option[ T ],
    ) : OtherSchema[ T ]

    trait CoproductReader[ T, R, D, DN, Trans <: Tuple ] {
        def read( from : Source, subtypes : R, trans : Trans ) : Option[ T ]
    }

    object CoproductReader {
        given subtypeReaderWithoutDiscriminator[ T, ST, N <: TypeName, S, Trans <: Tuple ](
            using
            st : RecursiveSchemaTranslator[ ST, S, Trans, OtherSchema ],
            str : SubtypeReader[ ST, N ],
        ) : CoproductReader[ T, Subtype[ T, ST, Unit, Unit, Unit, N, S ], Unit, Unit, Trans ] with {
            override def read(
                from: Source,
                subtypes: Subtype[ T, ST, Unit, Unit, Unit, N, S ],
                trans: Trans,
            ) : Option[ T ] = {
                val decoder = st.translate( subtypes.schema, trans )
                str
                  .read( from, subtypes, decoder )
                  .flatMap { v =>
                      if ( subtypes.validators.forall( _.isValid( v ) ) ) Some( subtypes.toSuper( v ) )
                      else None
                  }
                //                decoder( from.hcursor ) match {
                //                    case Left( e ) =>
                //                        None
                //                    case Right( v ) =>
                //                        if ( subtypes.validators.forall( _.isValid( v ) ) ) Some( subtypes.toSuper( v ) )
                //                        else None
                //                }
            }
        }

        given lazySubtypeReaderWithoutDiscriminator[ T, ST, N <: TypeName, S, Trans <: Tuple ](
            using
            sch : Schema.Aux[ ST, S ],
            tr: TransRetriever.Aux[ Trans, ST, S, OtherSchema ],
            str : SubtypeReader[ ST, N ],
        ) : CoproductReader[ T, LazySubtype[ T, ST, Unit, Unit, Unit, N ], Unit, Unit, Trans ] with {
            override def read(
                from: Source,
                subtypes: LazySubtype[ T, ST, Unit, Unit, Unit, N ],
                trans: Trans,
            ) : Option[ T ] = {
                val translator = tr.getTranslator( trans )
                val decoder = translator.translate( subtypes.schema )
                str
                  .read( from, subtypes, decoder )
                  .flatMap { v =>
                      if ( subtypes.validators.forall( _.isValid( v ) ) ) Some( subtypes.toSuper( v ) )
                      else None
                  }
                //                decoder( from.hcursor ) match {
                //                    case Left( e ) =>
                //                        None
                //                    case Right( v ) =>
                //                        if ( subtypes.validators.forall( _.isValid( v ) ) ) Some( subtypes.toSuper( v ) )
                //                        else None
                //                }
            }
        }

        given subtypeReaderWithDiscriminator[ T, ST, N <: TypeName, S, D, DN <: FieldName, DV <: D & Singleton, DS, Trans <: Tuple ](
            using
            dsp : SchemaProvider.Aux[ D, DS ],
            dst : SchemaTranslator[ D, DS, OtherSchema ],
            st : RecursiveSchemaTranslator[ ST, S, Trans, OtherSchema ],
            str : SubtypeReader[ ST, N ],
            vo : ValueOf[ DN ],
            dr : DiscrReader[ D ],
        ) : CoproductReader[ T, Subtype[ T, ST, D, DN, DV, N, S ], D, DN, Trans ] = {
            val dSch = dst.translate( dsp.provide )

            new CoproductReader[ T, Subtype[ T, ST, D, DN, DV, N, S ], D, DN, Trans ] {
                override def read(
                    from: Source,
                    subtypes: Subtype[ T, ST, D, DN, DV, N, S ],
                    trans: Trans,
                ): Option[ T ] = {
                    val discrName : String = subtypes.discriminatorName
                    val discrValue : D = subtypes.discriminatorValue

                    dr.read( from, discrName, dSch ) match {
                        case None => throw IllegalStateException( "" )
                        case Some( `discrValue` ) =>
                            val decoder = st.translate( subtypes.schema, trans )
                            str
                              .read( from, subtypes, decoder )
                              .flatMap { v =>
                                  if ( subtypes.validators.forall( _.isValid( v ) ) )
                                      Some( subtypes.toSuper( v ) )
                                  else None
                              }
                        //                            val stValue = decoder( from.hcursor ) match {
                        //                                case Left( e ) =>
                        //                                    None
                        //                                case Right( v ) =>
                        //                                    Some( v )
                        //                            }
                        //                            stValue.map( subtypes.toSuper )
                        case _ => None
                    }
                }
            }
        }

        given emptyReader[ T, D, DN, Trans <: Tuple ] : CoproductReader[ T, EmptyTuple, D, DN, Trans ] with {
            override def read(
                from: Source, subtypes: EmptyTuple, trans: Trans,
            ) : Option[ T ] = None
        }

        given tupleReader[ T, D, DN, H, Tail <: Tuple, Trans <: Tuple ](
            using
            h : CoproductReader[ T, H, D, DN, Trans ],
            t : CoproductReader[ T, Tail, D, DN, Trans ],
        ) : CoproductReader[ T, H *: Tail, D, DN, Trans ] with {
            override def read(
                from: Source, subtypes: H *: Tail, trans: Trans,
            ) : Option[ T ] = {
                h.read( from, subtypes.head, trans ) match {
                    case res@Some( v ) =>
                        res
                    case _ =>
                        t.read( from, subtypes.tail, trans )
                }
            }
        }
    }

    given decoderTranslator[ T, R <: Tuple, RV <: Tuple, D, DN, Trans <: Tuple ](
        using
        reader : CoproductReader[ T, R, D, DN, RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, OtherSchema ] *: Trans ],
    ) : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, OtherSchema ] with { self =>
        override def translate(
            schema: Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            trans : Trans,
        ): OtherSchema[ T ] = {
            val nextTrans : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, OtherSchema ] *: Trans = self *: trans
            buildCoproductDecoder[ T ]( source => reader.read( source, schema.shape.subtypeDescriptions, nextTrans ) )
        }
    }

}
