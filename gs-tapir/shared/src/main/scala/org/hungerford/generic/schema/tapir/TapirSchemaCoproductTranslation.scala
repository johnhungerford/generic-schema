package org.hungerford.generic.schema.tapir

import sttp.tapir.{FieldName as TapirFieldName, Schema as TapirSchema}
import sttp.tapir.SchemaType.{SBinary, SCoproduct, SDiscriminator, SRef, SString}
import sttp.tapir.Validator as TapirValidator
import org.hungerford.generic.schema.{ComplexSchema, Schema}
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, TypeName}
import org.hungerford.generic.schema.translation.{CI, Cached, RecursiveSchemaTranslator, SchemaCacheRetriever, SchemaTranslator}
import org.hungerford.generic.schema.product.field.FieldName
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.tapir.TapirValidatorTranslation
import org.hungerford.generic.schema.types.{ExistsFor, Partition}
import sttp.tapir.Schema.SName

import scala.reflect.ClassTag
import scala.util.NotGiven

trait TapirSchemaCoproductTranslation {

    trait TapirCoproductTranslator[ T, R, Trans <: Tuple ] {
        type Out

        def translate( shape : R, trans : Trans ) : Out
    }

    object TapirCoproductTranslator {
        type Aux[ T, R, O, Trans <: Tuple ] = TapirCoproductTranslator[ T, R, Trans ] { type Out = O }

        /**
         * Translate empty tuple of non-singleton subtypes
         */
        given [ T, Trans <: Tuple ] : TapirCoproductTranslator[ T, EmptyTuple, Trans ] with {
            type Out = (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ])

            override def translate( shape: EmptyTuple, trans : Trans ): Out = (Nil, ( t : T ) => None)
        }

        /**
         * Translate non-empty tuple of non-singleton subtypes
         */
        given [ T, H, Tail <: Tuple, Trans <: Tuple ](
            using
            current : TapirCoproductTranslator.Aux[ T, H, (TapirSchema[ _ ], T => Option[ TapirSchema[ _ ] ]), Trans ],
            next : TapirCoproductTranslator.Aux[ T, Tail, (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]), Trans ],
            ev1 : Partition[ IsSingletonSubtype, H *: Tail ],
            ev2 : ev1.MeetsCondition =:= EmptyTuple,
        ) : TapirCoproductTranslator[ T, H *: Tail, Trans ] with {
            type Out = (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ])

            override def translate(
                shape: H *: Tail,
                trans : Trans,
            ): (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]) = {
                val (nextSchemas, nextFromSuper) = next.translate( shape.tail, trans )
                val (currentSchema, currentFromSuper) = current.translate( shape.head, trans )
                val schemas = currentSchema +: nextSchemas
                val fromSuper = ( t : T ) => currentFromSuper( t ) match {
                    case res@Some( _ ) => res
                    case None => nextFromSuper( t )
                }
                (schemas, fromSuper)
            }
        }

        /**
         * Translate empty tuple of only singleton subtypes into a single enum string tapir schema
         */
        given emptySingleton[ T, Trans <: Tuple ] : TapirCoproductTranslator[ T, EmptyTuple, Trans ] with {
            type Out = (List[ String ], T => Boolean)

            override def translate(
                shape: EmptyTuple,
                trans : Trans,
            ): Out = (Nil, ( t : T ) => false)
        }

        /**
         * Translate non-empty tuple of only singleton subtypes into a single enum string tapir schema
         */
        given nonemptySingleton[ T, H, Tail <: Tuple, Trans <: Tuple ](
            using
            ev : ExistsFor[ IsSingletonSubtype, H *: Tail ],
            h : TapirCoproductTranslator.Aux[ T, H, (List[ String ], T => Boolean), Trans ],
            t : TapirCoproductTranslator.Aux[ T, Tail, (List[ String ], T => Boolean), Trans ],
        ) : TapirCoproductTranslator[ T, H *: Tail, Trans ] with {
            type Out = (List[ String ], T => Boolean)

            override def translate(
                shape: H *: Tail,
                trans : Trans,
            ): Out = {
                val (headValues, headMatcher) = h.translate( shape.head, trans )
                val (tailValues, tailMatcher) = t.translate( shape.tail, trans )

                (headValues ++ tailValues, ( t : T ) => headMatcher( t ) || tailMatcher( t ))
            }
        }

        /**
         * Translate tuple of mixed singleton and non-singleton subtypes
         */
        given [ T, R <: Tuple, IsSingle <: NonEmptyTuple, NotSingle <: NonEmptyTuple, Trans <: Tuple ](
            using
            part : Partition.Aux[ IsSingletonSubtype, R, IsSingle, NotSingle ],
            nst : TapirCoproductTranslator.Aux[ T, NotSingle, (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]), Trans ],
            st : TapirCoproductTranslator.Aux[ T, IsSingle, (List[ String ], T => Boolean), Trans ],
        ) : TapirCoproductTranslator[ T, R, Trans ] with {
            type Out = (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ])

            override def translate(
                shape: R,
                trans : Trans,
            ): (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]) = {
                val (isSingle, nonSingle) = part.filter( shape )
                val (singletonValues, singletonMatcher) = st.translate( isSingle, trans )
                val (nonsingleSchema, nonsingleMatcher) = nst.translate( nonSingle, trans )
                val singletonSchema = TapirSchema[ String ](
                    schemaType = SString(),
                    validator = TapirValidator.enumeration( singletonValues, v => Some( v ) ),
                )
                val matcher = ( t : T ) => if singletonMatcher( t ) then Some( singletonSchema ) else nonsingleMatcher( t )
                val schemas = singletonSchema +: nonsingleSchema
                (schemas, matcher)
            }
        }

        /**
         * Translate singleton subtype to string schema + matcher
         */
        given singletonSubtype[ T, ST <: Singleton, D, DN, DV, N <: TypeName, SN <: TypeName, Trans <: Tuple ] :
          TapirCoproductTranslator[ T, Subtype[ T, ST, D, DN, DV, N, SingletonShape[ ST, SN ] ], Trans ] with {
            type Out = (List[ String ], T => Boolean)

            override def translate(
                shape: Subtype[ T, ST, D, DN, DV, N, SingletonShape[ ST, SN ] ],
                trans : Trans,
            ): (List[ String ], T => Boolean) = {
                (List[ String ]( shape.schema.shape.name ), ( t : T ) => shape.fromSuper( t ).isDefined)
            }
        }

        /**
         * Translate non-singleton subtype to tapir schema + matcher
         */
        given nonSingletonSubtype[ T, ST, D, DN, DV, N <: TypeName, S, Trans <: Tuple ](
          using
          stst : RecursiveSchemaTranslator[ ST, S, Trans, TapirSchema ],
          ev : NotGiven[ IsSingletonSubtype[ Subtype[ T, ST, D, DN, DV, N, S ] ] ],
        ) : TapirCoproductTranslator[ T, Subtype[ T, ST, D, DN, DV, N, S ], Trans ] with {
            type Out = (TapirSchema[ _ ], T => Option[ TapirSchema[ _ ] ])

            override def translate(
                shape: Subtype[ T, ST, D, DN, DV, N, S ],
                trans : Trans,
            ): (TapirSchema[ _ ], T => Option[ TapirSchema[ _ ] ]) = {
                val tSchema = stst.translate( shape.schema, trans )
                val fromSuper = ( v: T ) => shape.fromSuper( v ).map( _ => tSchema )
                (tSchema, fromSuper)
            }
        }

        /**
         * Translate lazy subtype to tapir schema + matcher
         */
        given lazySubtype[ T, ST, D, DN, DV, N <: TypeName, Trans <: Tuple ](
            using
            scr : SchemaCacheRetriever.Aux[ Trans, ST, (Option[ String ], String) ],
        ) : TapirCoproductTranslator[ T, LazySubtype[ T, ST, D, DN, DV, N ], Trans ] with {
            type Out = (TapirSchema[ _ ], T => Option[ TapirSchema[ _ ] ])

            override def translate(
                shape: LazySubtype[ T, ST, D, DN, DV, N ],
                trans : Trans,
            ): (TapirSchema[ _ ], T => Option[ TapirSchema[ _ ] ]) = {
                val (nameOpt, refName) = scr.getter( trans ).get()
                val tSchema = TapirSchema[ ST ](SRef[ ST ]( SName( nameOpt.getOrElse( refName ) ) ), Some( SName( refName ) ) )
                val fromSuper = ( v: T ) => shape.fromSuper( v ).map( _ => tSchema )
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

    given tapirSchemaCoproductTranslation[ T : ClassTag, R <: NonEmptyTuple, RV <: NonEmptyTuple, D, DN, Trans <: Tuple ](
        using
        ev : NotGiven[ ExistsFor[ IsSingletonSubtype, R ] ],
        cpt : TapirCoproductTranslator.Aux[ T, R, (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]), CI[ T, (Option[ String ], String) ] *: Trans ],
        dt : TapirDiscriminatorTranslation.Aux[ T, D, DN, R, Option[ SDiscriminator ] ],
        vd : TapirValidatorTranslation[ T ],
    ) : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, TapirSchema ] with {
        override def translate(
            schema: Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            trans: Trans,
        ): TapirSchema[ T ] = {
            lazy val res : TapirSchema[ T ] = {

                val cachedItem = CI.of[ T ]( schema.name, summon[ ClassTag[ T ] ].runtimeClass.getSimpleName )
                val (subtypes, fromSuper) = cpt.translate( schema.shape.subtypeDescriptions, cachedItem *: trans )
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
            res
        }
    }

    given tapirSchemaSingletonCoproductTranslation[ T, R <: NonEmptyTuple, RV <: NonEmptyTuple, D, DN, Trans <: Tuple ](
        using
        ev : ExistsFor[ IsSingletonSubtype, R ],
    ) : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Trans, TapirSchema ] with {
        override def translate(
            schema: Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            trans : Trans,
        ): TapirSchema[ T ] = {
            val stList : List[ (T, T => Option[ String ]) ] = schema.shape.subtypeDescriptions.toList map {
                case Subtype( _, ComplexSchema( SingletonShape( caseName, caseValue ), _, _, _, _, _ ), _, matcher, _, _, _, _, _, _, _ ) =>
                    (caseValue.asInstanceOf[ T ], ( t : T ) => matcher.asInstanceOf[ Function1[ T, Option[ Any ] ] ]( t ).map( _ => caseName.asInstanceOf[ String ] ) )
            }

            val encoder : T => Option[ String ] = stList.foldLeft( ( t : T ) => None : Option[ String ] ) { ( lastFn, nextTup ) =>
                val (_, fn) = nextTup
                ( t : T ) => fn( t ) orElse lastFn( t )
            }

            TapirSchema(
                SString(),
                schema.name.map( n => TapirSchema.SName( n ) ),
                false,
                schema.genericDescription,
                None,
                None,
                schema.genericExamples.headOption,
                schema.deprecated,
                TapirValidator.enumeration( stList.map( _._1 ), encoder ),
            )
        }
    }

    given [ Trans <: Tuple, T : ClassTag, R <: Tuple, RV <: Tuple, D, DN ] : Cached[ Trans, T ] with {
        type In = Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ]
        type Out = (Option[ String ], String)

        override def cacheItem(
            value: Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ]
        ): (Option[ String ], String) = {
            (value.name, summon[ ClassTag[ T ] ].runtimeClass.getSimpleName)
        }
    }

}
