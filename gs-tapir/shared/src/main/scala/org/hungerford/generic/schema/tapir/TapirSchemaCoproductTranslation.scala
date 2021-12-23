package org.hungerford.generic.schema.tapir

import sttp.tapir.{Schema as TapirSchema, FieldName as TapirFieldName}
import sttp.tapir.SchemaType.{SCoproduct, SDiscriminator, SRef}
import sttp.tapir.Validator as TapirValidator
import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.product.field.FieldName
import org.hungerford.generic.schema.tapir.TapirValidatorTranslation

trait TapirSchemaCoproductTranslation {

    trait TapirCoproductTranslator[ T, R ] {
        type Out

        def translate( shape : R ) : Out
    }

    object TapirCoproductTranslator {
        type Aux[ T, R, O ] = TapirCoproductTranslator[ T, R ] { type Out = O }
        given [ T ] : TapirCoproductTranslator[ T, EmptyTuple ] with {
            type Out = (Nil.type, T => Option[ TapirSchema[ _ ] ])

            override def translate( shape: EmptyTuple ): Out = (Nil, ( t : T ) => None)
        }

        given [ T, H, Tail <: Tuple ](
            using
            current : TapirCoproductTranslator.Aux[ T, H, (TapirSchema[ _ ], T => Option[ TapirSchema[ _ ] ]) ],
            next : TapirCoproductTranslator.Aux[ T, Tail, (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]) ],
        ) : TapirCoproductTranslator[ T, H *: Tail ] with {
            type Out = (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ])

            override def translate(
                shape: H *: Tail
            ): (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]) = {
                val (nextSchemas, nextFromSuper) = next.translate( shape.tail )
                val (currentSchema, currentFromSuper) = current.translate( shape.head )
                val schemas = currentSchema +: nextSchemas
                val fromSuper = ( t : T ) => currentFromSuper( t ) match {
                    case res@Some( _ ) => res
                    case None => nextFromSuper( t )
                }
                (schemas, fromSuper)
            }
        }

        given [ T, ST, D, DN, DV, N <: TypeName, S ](
          using
            stst : SchemaTranslator[ ST, S, TapirSchema ],
        ) : TapirCoproductTranslator[ T, Subtype.Aux[ T, ST, D, DN, DV, N, S ] ] with {
            type Out = (TapirSchema[ ST ], T => Option[ TapirSchema[ ST ] ])

            override def translate(
                shape: Subtype.Aux[ T, ST, D, DN, DV, N, S ]
            ): (TapirSchema[ ST ], T => Option[ TapirSchema[ ST ] ]) = {
                val tSchema = stst.translate( shape.schema )
                val fromSuper = ( v : T ) => shape.fromSuper( v ).map( _ => tSchema )
                (tSchema, fromSuper)
            }
        }
    }

    trait TapirDiscriminatorTranslation[ T, D, DN, R ] {
        type Out

        def translate( shape : R ) : Out
    }

    object TapirDiscriminatorTranslation {
        type Aux[ T, D, DN, R, O ] = TapirDiscriminatorTranslation[ T, D, DN, R ] { type Out = O }

        given noDiscriminator[ T, DN, R ] : TapirDiscriminatorTranslation[ T, Unit, DN, R ] with {
            type Out = Option[ SDiscriminator ]

            override def translate( shape: R ) : Option[ SDiscriminator ] = None
        }

        inline given someDiscriminatorFromEmptyTuple[ T, D, DN <: FieldName ](
            using
            vo : ValueOf[ DN ],
        ) : TapirDiscriminatorTranslation[ T, D, DN, EmptyTuple ] with {
            type Out = Option[ SDiscriminator ]

            override def translate( shape: EmptyTuple ) : Out =
                Some( SDiscriminator( TapirFieldName( vo.value, vo.value ), Map.empty[ String, SRef[ _ ] ] ) )
        }

        given someDiscriminatorFromTuple[ T, D, DN, H, Tail <: Tuple ](
            using
            current : TapirDiscriminatorTranslation.Aux[ T, D, DN, H, (String, SRef[ _ ]) ],
            next : TapirDiscriminatorTranslation.Aux[ T, D, DN, Tail, Option[ SDiscriminator ] ]
        ) : TapirDiscriminatorTranslation[ T, D, DN, H *: Tail ] with {
            override type Out = Option[ SDiscriminator ]

            override def translate( shape: H *: Tail ): Out = {
                val srefTuple = current.translate( shape.head )
                val nextDiscr = next.translate( shape.tail )
                nextDiscr.map( discr => discr.copy( mapping = discr.mapping + srefTuple )  )
            }
        }
    }

    given tapirSchemaCoproductTranslation[ T, R <: Tuple, RV <: Tuple, D, DN ](
        using
        cpt : TapirCoproductTranslator.Aux[ T, R, (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]) ],
        dt : TapirDiscriminatorTranslation.Aux[ T, D, DN, R, Option[ SDiscriminator ] ],
        vd : TapirValidatorTranslation[ T ],
    ) : SchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], TapirSchema ] with {
        override def translate(
            schema: Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
        ): TapirSchema[ T ] = {
            val (subtypes, fromSuper) = cpt.translate( schema.shape.subtypeDescriptions )
            val discr = dt.translate( schema.shape.subtypeDescriptions )

            TapirSchema(
                SCoproduct( subtypes, discr )( fromSuper ),
                schema.name.map( n => TapirSchema.SName( n ) ),
                false,
                schema.genericDescription,
                None,
                None,
                schema.genericExamples.headOption,
                schema.deprecated,
                schema.genericValidators
                  .foldLeft( TapirValidator.pass[ T ] )( (validr, nextV ) => validr and vd.translate( nextV ) ),
            )
        }
    }

}
