package org.hungerford.generic.schema.upickle

import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.SchemaProvider
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{TypeName, Subtype}
import org.hungerford.generic.schema.translation.SchemaTranslator

import org.hungerford.generic.schema.product.field.FieldName

import scala.util.{Failure, Success, Try}

import ujson.Value
import upickle.default.{read as upkRead, write as upkWrite, *}

trait UPickleCoproductSchemaTranslation {

    trait CoproductReader[ Source, R, D, DN ] {
        type Out

        def read( from : Source, subtypes : R ) : Out
    }

    object CoproductReader {
        type Aux[ Source, R, D, DN, O ] = CoproductReader[ Source, R, D, DN ] { type Out = O }

        given subtypeReaderWithoutDiscriminator[ T, ST, N <: TypeName, S ](
            using
            st : SchemaTranslator[ ST, S, ReadWriter ]
        ) : CoproductReader[ Value.Value, Subtype[ T, ST, Unit, Nothing, Unit, N, S ], Unit, Nothing ] with {
            type Out = Option[ T ]

            override def read(
                from: Value.Value,
                subtypes: Subtype[ T, ST, Unit, Nothing, Unit, N, S ]
            ) : subtypeReaderWithoutDiscriminator.this.Out = {
                given reader : Reader[ ST ] = st.translate( subtypes.schema )
                Try( upkRead[ ST ]( from ) ).toOption flatMap { t =>
                    if ( subtypes.validators.forall( _.isValid( t ) ) ) Some( subtypes.toSuper( t ) )
                    else None
                }
            }
        }

        given subtypeReaderWithDiscriminator[ T, ST, N <: TypeName, S, D, DN <: FieldName, DV <: D & Singleton, DS ](
            using
            dsp : SchemaProvider.Aux[ D, DS ],
            dst : SchemaTranslator[ D, DS, ReadWriter ],
            st : SchemaTranslator[ ST, S, ReadWriter ],
            vo : ValueOf[ DN ],
        ) : CoproductReader.Aux[ Value.Value, Subtype[ T, ST, D, DN, DV, N, S ], D, DN, Option[ T ] ] = {
            val discrFieldName = vo.value
            given dSch : Reader[ D ] = dst.translate( dsp.provide )

            new CoproductReader[ Value.Value, Subtype[ T, ST, D, DN, DV, N, S ], D, DN ] {
                type Out = Option[ T ]

                override def read(
                    from: Value.Value,
                    subtypes: Subtype[ T, ST, D, DN, DV, N, S ]
                ): Out = {
                    val discrValue = subtypes.discriminatorValue
                    Try( upkRead[ D ]( from( discrFieldName ) ) ) match {
                        case Success( `discrValue` ) =>
                            given stSch : Reader[ ST ] = st.translate( subtypes.schema )
                            Try( upkRead[ ST ]( from ) ).toOption.map( subtypes.toSuper )
                        case _ => None
                    }
                }
            }
        }

        given emptyReader[ T, D, DN ] : CoproductReader[ Value.Value, EmptyTuple, D, DN ] with {
            type Out = Option[ T ]

            override def read(
                from: Value.Value, subtypes: EmptyTuple,
            ) : Out = None
        }

        given tupleReader[ T, D, DN, H, Tail <: Tuple ](
            using
            h : CoproductReader.Aux[ Value.Value, H, D, DN, Option[ T ] ],
            t : CoproductReader.Aux[ Value.Value, Tail, D, DN, Option[ T ] ],
        ) : CoproductReader[ Value.Value, H *: Tail, D, DN ] with {
            type Out = Option[ T ]

            override def read(
                from: Value.Value, subtypes: H *: Tail,
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
            st : SchemaTranslator[ ST, STS, ReadWriter ],
        ) : CoproductWriter[ T, Subtype[ T, ST, D, DN, DV, N, STS ] ] with {
            type Out = Option[ Value.Value ]

            override def write(
                value: T,
                informedBy: Subtype[ T, ST, D, DN, DV, N, STS ],
            ) : Out = {
                informedBy.fromSuper( value ).flatMap( stVal => {
                    given encoder : Writer[ ST ] = st.translate( informedBy.schema )
                    informedBy.fromSuper( value ).flatMap( st => {
                        Try( writeJs( st ) ).toOption
                    } )
                } )
            }
        }

        given emptyWriter[ T ] : CoproductWriter[ T, EmptyTuple ] with {
            type Out = Option[ Value.Value ]

            override def write( value: T, informedBy : EmptyTuple ) : Out = None
        }

        given tupleWriter[ T, H, Tail <: Tuple ](
            using
            h : CoproductWriter.Aux[ T, H, Option[ Value.Value ] ],
            t : CoproductWriter.Aux[ T, Tail, Option[ Value.Value ] ],
        ) : CoproductWriter[ T, H *: Tail ] with {
            type Out = Option[ Value.Value ]

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
        reader : CoproductReader.Aux[ Value.Value, R, D, DN, Option[ T ] ],
        writer : CoproductWriter.Aux[ T, R, Option[ Value.Value ] ],
    ) : SchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], ReadWriter ] with {
        override def translate(
            schema: Aux[ T, CoproductShape[ T, R, RV, D, DN ] ]
        ): ReadWriter[ T ] = {
            val encoder: T => Value = ( a: T ) => writer.write( a, schema.shape.subtypeDescriptions ).get

            val decoder: Value => T = ( c: Value.Value ) => reader.read( c, schema.shape.subtypeDescriptions ).get

            readwriter[ Value.Value ].bimap[ T ]( encoder, decoder )
        }
    }

}
