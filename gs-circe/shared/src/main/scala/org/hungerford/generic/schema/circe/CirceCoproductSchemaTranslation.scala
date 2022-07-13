package org.hungerford.generic.schema.circe

import io.circe.Decoder.Result
import io.circe.{Codec, Decoder, Encoder, HCursor, Json}
import io.circe.DecodingFailure
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.{Schema, SchemaProvider}
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, TypeName}
import org.hungerford.generic.schema.translation.{RecursiveSchemaTranslator, SchemaTranslator}
import org.hungerford.generic.schema.product.field.FieldName

import scala.util.{Failure, Success, Try}

trait CirceCoproductSchemaTranslation {

    trait CoproductReader[ Source, R, D, DN, Trans <: Tuple ] {
        type Out

        def read( from : Source, subtypes : R, trans : Trans ) : Out
    }

    object CoproductReader {
        type Aux[ Source, R, D, DN, O, Trans <: Tuple ] = CoproductReader[ Source, R, D, DN, Trans ] { type Out = O }

        given subtypeReaderWithoutDiscriminator[ T, ST, N <: TypeName, S, Trans <: Tuple ](
            using
            st : RecursiveSchemaTranslator[ ST, S, Trans, Codec ],
            vo : ValueOf[ N ],
        ) : CoproductReader[ Json, Subtype[ T, ST, Unit, Unit, Unit, N, S ], Unit, Unit, Trans ] with {
            type Out = Option[ T ]

            override def read(
                from: Json,
                subtypes: Subtype[ T, ST, Unit, Unit, Unit, N, S ],
                trans: Trans,
            ) : subtypeReaderWithoutDiscriminator.this.Out = {
                println( s"Reading: ${vo.value}" )
                val decoder = st.translate( subtypes.schema, trans )
                decoder( from.hcursor ) match {
                    case Left( e ) =>
                        println( "FAILED SUBTYPE READING WITHOUT DISCR:" )
                        println( e )
                        None
                    case Right( v ) =>
                        println( "SUCCESSFUL SUBTYPE READING WITHOUT DISCR:" )
                        println( v )
                        if ( subtypes.validators.forall( _.isValid( v ) ) ) Some( subtypes.toSuper( v ) )
                        else None
                }
            }
        }

        given lazySubtypeReaderWithoutDiscriminator[ T, ST, N <: TypeName, S, Trans <: Tuple ](
            using
            sch : Schema.Aux[ ST, S ],
            tr: TransRetriever.Aux[ Trans, ST, S, Decoder ],
            vo: ValueOf[ N ],
        ) : CoproductReader[ Json, LazySubtype[ T, ST, Unit, Unit, Unit, N ], Unit, Unit, Trans ] with {
            type Out = Option[ T ]

            override def read(
                from: Json,
                subtypes: LazySubtype[ T, ST, Unit, Unit, Unit, N ],
                trans: Trans,
            ) : Out = {
                val translator = tr.getTranslator( trans ).asInstanceOf[ SchemaTranslator[ ST, S, Decoder ] ]
                val decoder = translator.translate( subtypes.schema )
                println( s"Reading (lazy subtype): ${vo.value}" )
                decoder( from.hcursor ) match {
                    case Left( e ) =>
                        println( "FAILED LAZY SUBTYPE READING WITHOUT DISCR" )
                        println( from )
                        println( e )
                        None
                    case Right( v ) =>
                        println( "SUCCESSFUL LAZY SUBTYPE READING WITHOUT DISCR" )
                        println( v )
                        if ( subtypes.validators.forall( _.isValid( v ) ) ) Some( subtypes.toSuper( v ) )
                        else None
                }
            }
        }

        given subtypeReaderWithDiscriminator[ T, ST, N <: TypeName, S, D, DN <: FieldName, DV <: D & Singleton, DS, Trans <: Tuple ](
            using
            dsp : SchemaProvider.Aux[ D, DS ],
            dst : SchemaTranslator[ D, DS, Codec ],
            st : RecursiveSchemaTranslator[ ST, S, Trans, Codec ],
            vo : ValueOf[ DN ],
        ) : CoproductReader.Aux[ Json, Subtype[ T, ST, D, DN, DV, N, S ], D, DN, Option[ T ], Trans ] = {
            val discrFieldName = vo.value
            val dSch = dst.translate( dsp.provide )

            new CoproductReader[ Json, Subtype[ T, ST, D, DN, DV, N, S ], D, DN, Trans ] {
                type Out = Option[ T ]

                override def read(
                    from: Json,
                    subtypes: Subtype[ T, ST, D, DN, DV, N, S ],
                    trans: Trans,
                ): Out = {
                    from.hcursor.get[ D ]( discrFieldName )( dSch ) match {
                        case Left( e ) =>
                            println( "UNABLE TO GET DSCRFIELDNAME" )
                            throw e
                        case Right( `discrFieldName` ) =>
                            val decoder = st.translate( subtypes.schema, trans )
                            val stValue = decoder( from.hcursor ) match {
                                case Left( e ) =>
                                    println( "SDFSDFSFD" )
                                    None
                                case Right( v ) =>
                                    Some( v )
                            }
                            stValue.map( subtypes.toSuper )
                        case _ => None
                    }
                }
            }
        }

        given emptyReader[ T, D, DN, Trans <: Tuple ] : CoproductReader[ Json, EmptyTuple, D, DN, Trans ] with {
            type Out = Option[ T ]

            override def read(
                from: Json, subtypes: EmptyTuple, trans: Trans,
            ) : Out = None
        }

        given tupleReader[ T, D, DN, H, Tail <: Tuple, Trans <: Tuple ](
            using
            h : CoproductReader.Aux[ Json, H, D, DN, Option[ T ], Trans ],
            t : CoproductReader.Aux[ Json, Tail, D, DN, Option[ T ], Trans ],
        ) : CoproductReader[ Json, H *: Tail, D, DN, Trans ] with {
            type Out = Option[ T ]

            override def read(
                from: Json, subtypes: H *: Tail, trans: Trans,
            ) : Out = {
                h.read( from, subtypes.head, trans ) match {
                    case res@Some( v ) =>
                        println( "SUCCESSFUL SUBTYPES READING:" )
                        println( v )
                        res
                    case _ =>
                        println( "FAILED SUBTYPES READING" )
                        t.read( from, subtypes.tail, trans )
                }
            }
        }
    }

    trait CoproductWriter[ V, R, Trans <: Tuple ] {
        type Out

        def write( value : V, informedBy : R, trans : Trans ) : Out
    }

    object CoproductWriter {
        type Aux[ T, R, O, Trans <: Tuple ] = CoproductWriter[ T, R, Trans ] { type Out = O }

        given subtypeWriter[ T, ST, D, DN, DV, N <: TypeName, STS, Trans <: Tuple ](
            using
            st : RecursiveSchemaTranslator[ ST, STS, Trans, Encoder ],
            vo : ValueOf[ N ],
        ) : CoproductWriter[ T, Subtype[ T, ST, D, DN, DV, N, STS ], Trans ] with {
            type Out = Option[ Json ]

            override def write(
                value: T,
                informedBy: Subtype[ T, ST, D, DN, DV, N, STS ],
                trans: Trans,
            ) : Out = {
//                println( s"Writing subtype: ${vo.value}" )
                informedBy.fromSuper( value ).map( stVal => {
                    val encoder = st.translate( informedBy.schema, trans )
                    encoder( stVal )
                } )
            }
        }

        given lazySubtypeWriter[ T, ST, D, DN, DV, N <: TypeName, STS, Trans <: Tuple ](
            using
            sch : Schema.Aux[ ST, STS ],
            tr : TransRetriever[ Trans, ST, Encoder ],
            vo : ValueOf[ N ],
        ) : CoproductWriter[ T, LazySubtype[ T, ST, D, DN, DV, N ], Trans ] with {
            type Out = Option[ Json ]

            override def write(
                value: T,
                informedBy: LazySubtype[ T, ST, D, DN, DV, N ],
                trans: Trans,
            ) : Out = {
//                println( s"Writing lazy subtype: ${vo.value}" )
                informedBy.fromSuper( value ).map( stVal => {
                    val encoder = tr.getTranslator( trans ).asInstanceOf[ SchemaTranslator[ ST, STS, Encoder ] ].translate( sch )
                    encoder( stVal )
                } )
            }
        }

        given emptyWriter[ T, Trans <: Tuple ] : CoproductWriter[ T, EmptyTuple, Trans ] with {
            type Out = Option[ Json ]

            override def write( value: T, informedBy : EmptyTuple, trans: Trans ): Out = None
        }

        given tupleWriter[ T, H, Tail <: Tuple, Trans <: Tuple ](
            using
            h : CoproductWriter.Aux[ T, H, Option[ Json ], Trans ],
            t : CoproductWriter.Aux[ T, Tail, Option[ Json ], Trans ],
        ) : CoproductWriter[ T, H *: Tail, Trans ] with {
            type Out = Option[ Json ]

            override def write( value: T, informedBy: H *: Tail, trans: Trans ) : Out = {
                h.write( value, informedBy.head, trans ) match {
                    case res@Some( _ ) => res
                    case _ => t.write( value, informedBy.tail, trans )
                }
            }
        }
    }

    given decoderTranslator[ T, R <: Tuple, RV <: Tuple, D, DN, Trans <: Tuple ](
        using
        reader : CoproductReader.Aux[ Json, R, D, DN, Option[ T ], RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, Decoder ] *: Trans ],
    ) : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, Decoder ] with { self =>
        override def translate(
            schema: Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            trans : Trans,
        ): Decoder[ T ] = {
            println(schema.shape)
            ( c: HCursor ) => Try( reader.read( c.value, schema.shape.subtypeDescriptions, self *: trans ) ) match {
                case Failure( exception : DecodingFailure ) => Left( exception )
                case Failure( exception ) => Left( DecodingFailure.fromThrowable( exception, Nil ) )
                case Success( value ) => value match {
                    case Some( v ) => Right( v )
                    case None => Left( DecodingFailure.apply( s"unable to decode any of ${schema.shape.subtypeDescriptions.size} subtypes from json", Nil ) )
                }
            }
        }
    }

    given encoderTranslator[ T, R <: Tuple, RV <: Tuple, D, DN, Trans <: Tuple ](
        using
        writer : CoproductWriter.Aux[ T, R, Option[ Json ], RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, Encoder ] *: Trans ],
    ) : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, Encoder ] with { self =>
        override def translate(
            schema: Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            trans : Trans,
        ): Encoder[ T ] = {
            ( a: T ) => writer.write( a, schema.shape.subtypeDescriptions, self *: trans ).get
        }
    }

}
