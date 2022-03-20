package org.hungerford.generic.schema.circe

import io.circe.Decoder.Result
import io.circe.{Codec, Decoder, Encoder, HCursor, Json}
import io.circe.DecodingFailure
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.SchemaProvider
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{TypeName, Subtype}
import org.hungerford.generic.schema.translation.SchemaTranslator

import org.hungerford.generic.schema.product.field.FieldName

import scala.util.{Failure, Success, Try}

trait CirceCoproductSchemaTranslation {

    trait CoproductReader[ Source, R, D, DN ] {
        type Out

        def read( from : Source, subtypes : R ) : Out
    }

    object CoproductReader {
        type Aux[ Source, R, D, DN, O ] = CoproductReader[ Source, R, D, DN ] { type Out = O }

        given subtypeReaderWithoutDiscriminator[ T, ST, N <: TypeName, S ](
            using
            st : SchemaTranslator[ ST, S, Codec ]
        ) : CoproductReader[ Json, Subtype[ T, ST, Unit, Nothing, Unit, N, S ], Unit, Nothing ] with {
            type Out = Option[ T ]

            override def read(
                from: Json,
                subtypes: Subtype[ T, ST, Unit, Nothing, Unit, N, S ]
            ) : subtypeReaderWithoutDiscriminator.this.Out = {
                val decoder = st.translate( subtypes.schema )
                decoder( from.hcursor ) match {
                    case Left( e ) => None
                    case Right( v ) =>
                        if ( subtypes.validators.forall( _.isValid( v ) ) ) Some( subtypes.toSuper( v ) )
                        else None
                }
            }
        }

        given subtypeReaderWithDiscriminator[ T, ST, N <: TypeName, S, D, DN <: FieldName, DV <: D & Singleton, DS ](
            using
            dsp : SchemaProvider.Aux[ D, DS ],
            dst : SchemaTranslator[ D, DS, Codec ],
            st : SchemaTranslator[ ST, S, Codec ],
            vo : ValueOf[ DN ],
        ) : CoproductReader.Aux[ Json, Subtype[ T, ST, D, DN, DV, N, S ], D, DN, Option[ T ] ] = {
            val discrFieldName = vo.value
            val dSch = dst.translate( dsp.provide )

            new CoproductReader[ Json, Subtype[ T, ST, D, DN, DV, N, S ], D, DN ] {
                type Out = Option[ T ]

                override def read(
                    from: Json,
                    subtypes: Subtype[ T, ST, D, DN, DV, N, S ]
                ): Out = {
                    from.hcursor.get[ D ]( discrFieldName )( dSch ) match {
                        case Left( e ) => throw e
                        case Right( `discrFieldName` ) =>
                            val decoder = st.translate( subtypes.schema )
                            val stValue = decoder( from.hcursor ) match {
                                case Left( e ) => None
                                case Right( v ) => Some( v )
                            }
                            stValue.map( subtypes.toSuper )
                        case _ => None
                    }
                }
            }
        }

        given emptyReader[ T, D, DN ] : CoproductReader[ Json, EmptyTuple, D, DN ] with {
            type Out = Option[ T ]

            override def read(
                from: Json, subtypes: EmptyTuple,
            ) : Out = None
        }

        given tupleReader[ T, D, DN, H, Tail <: Tuple ](
            using
            h : CoproductReader.Aux[ Json, H, D, DN, Option[ T ] ],
            t : CoproductReader.Aux[ Json, Tail, D, DN, Option[ T ] ],
        ) : CoproductReader[ Json, H *: Tail, D, DN ] with {
            type Out = Option[ T ]

            override def read(
                from: Json, subtypes: H *: Tail,
            ) : Out = {
                h.read( from, subtypes.head ) match {
                    case res@Some( _ ) => res
                    case _ => t.read( from, subtypes.tail )
                }
            }
        }
    }

    trait CoproductWriter[ V, R ] {
        type Out

        def write( value : V, informedBy : R ) : Out
    }

    object CoproductWriter {
        type Aux[ T, R, O ] = CoproductWriter[ T, R ] { type Out = O }

        given subtypeWriter[ T, ST, D, DN, DV, N <: TypeName, STS ](
            using
            st : SchemaTranslator[ ST, STS, Codec ],
        ) : CoproductWriter[ T, Subtype[ T, ST, D, DN, DV, N, STS ] ] with {
            type Out = Option[ Json ]

            override def write(
                value: T,
                informedBy: Subtype[ T, ST, D, DN, DV, N, STS ],
            ) : Out = {
                informedBy.fromSuper( value ).map( stVal => {
                    val encoder = st.translate( informedBy.schema )
                    encoder( stVal )
                } )
            }
        }

        given emptyWriter[ T ] : CoproductWriter[ T, EmptyTuple ] with {
            type Out = Option[ Json ]

            override def write( value: T, informedBy : EmptyTuple ): Out = None
        }

        given tupleWriter[ T, H, Tail <: Tuple ](
            using
            h : CoproductWriter.Aux[ T, H, Option[ Json ] ],
            t : CoproductWriter.Aux[ T, Tail, Option[ Json ] ],
        ) : CoproductWriter[ T, H *: Tail ] with {
            type Out = Option[ Json ]

            override def write( value: T, informedBy: H *: Tail ) : Out = {
                h.write( value, informedBy.head ) match {
                    case res@Some( _ ) => res
                    case _ => t.write( value, informedBy.tail )
                }
            }
        }
    }

    given [ T, R <: Tuple, RV <: Tuple, D, DN ](
        using
        reader : CoproductReader.Aux[ Json, R, D, DN, Option[ T ] ],
        writer : CoproductWriter.Aux[ T, R, Option[ Json ] ],
    ) : SchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Codec ] with {
        override def translate(
            schema: Aux[ T, CoproductShape[ T, R, RV, D, DN ] ]
        ): Codec[ T ] = {
            val encoder: Encoder[ T ] = ( a: T ) => writer.write( a, schema.shape.subtypeDescriptions ).get

            val decoder: Decoder[ T ] = ( c: HCursor ) => Try( reader.read( c.value, schema.shape.subtypeDescriptions ) ) match {
                case Failure( exception : DecodingFailure ) => Left( exception )
                case Failure( exception ) => Left( DecodingFailure.fromThrowable( exception, Nil ) )
                case Success( value ) => value match {
                    case Some( v ) => Right( v )
                    case None => Left( DecodingFailure.apply( s"unable to decode any of ${schema.shape.subtypeDescriptions.size} subtypes from json", Nil ) )
                }
            }
            Codec.from[ T ]( decoder, encoder )
        }
    }

}
