package org.hungerford.generic.schema.tapir

import sttp.tapir.{FieldName as TapirFieldName, Schema as TapirSchema}
import sttp.tapir.SchemaType.{SchemaWithValue, SBinary, SCoproduct, SDiscriminator, SRef, SString}
import sttp.tapir.Validator as TapirValidator
import org.hungerford.generic.schema.{ComplexSchema, Schema}
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.coproduct.subtype.{LazySubtype, Subtype, TypeName}
import org.hungerford.generic.schema.translation.{CI, Cacher, RecursiveSchemaTranslator, SchemaTranslator, TypeCache, TypeCacheRetriever}
import org.hungerford.generic.schema.product.field.FieldName
import org.hungerford.generic.schema.singleton.SingletonShape
import org.hungerford.generic.schema.tapir.TapirValidatorTranslation
import org.hungerford.generic.schema.types.{ExistsFor, Partition}
import sttp.tapir.Schema.SName

import scala.reflect.ClassTag
import scala.util.NotGiven

trait TapirSchemaCoproductTranslation {

    trait TapirCoproductTranslator[ T, R, Cache <: TypeCache ] {
        type Out

        def translate( shape : R, cache : Cache ) : Out
    }

    object TapirCoproductTranslator {
        type Aux[ T, R, O, Cache <: TypeCache ] = TapirCoproductTranslator[ T, R, Cache ] { type Out = O }

        /**
         * Cachelate empty tuple of non-singleton subtypes
         */
        given [ T, Cache <: TypeCache ] : TapirCoproductTranslator[ T, EmptyTuple, Cache ] with {
            type Out = (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ])

            override def translate( shape: EmptyTuple, cache : Cache ): Out = (Nil, ( t : T ) => None)
        }

        /**
         * Cachelate non-empty tuple of non-singleton subtypes
         */
        given [ T, H, Tail <: Tuple, Cache <: TypeCache ](
            using
            current : TapirCoproductTranslator.Aux[ T, H, (TapirSchema[ _ ], T => Option[ TapirSchema[ _ ] ]), Cache ],
            next : TapirCoproductTranslator.Aux[ T, Tail, (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]), Cache ],
            ev1 : Partition[ IsSingletonSubtype, H *: Tail ],
            ev2 : ev1.MeetsCondition =:= EmptyTuple,
        ) : TapirCoproductTranslator[ T, H *: Tail, Cache ] with {
            type Out = (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ])

            override def translate(
                shape: H *: Tail,
                cache : Cache,
            ): (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]) = {
                val (nextSchemas, nextFromSuper) = next.translate( shape.tail, cache )
                val (currentSchema, currentFromSuper) = current.translate( shape.head, cache )
                val schemas = currentSchema +: nextSchemas
                val fromSuper = ( t : T ) => currentFromSuper( t ) match {
                    case res@Some( _ ) => res
                    case None => nextFromSuper( t )
                }
                (schemas, fromSuper)
            }
        }

        /**
         * Cachelate empty tuple of only singleton subtypes into a single enum string tapir schema
         */
        given emptySingleton[ T, Cache <: TypeCache ] : TapirCoproductTranslator[ T, EmptyTuple, Cache ] with {
            type Out = (List[ String ], T => Boolean)

            override def translate(
                shape: EmptyTuple,
                cache : Cache,
            ): Out = (Nil, ( t : T ) => false)
        }

        /**
         * Cachelate non-empty tuple of only singleton subtypes into a single enum string tapir schema
         */
        given nonemptySingleton[ T, H, Tail <: Tuple, Cache <: TypeCache ](
            using
            ev : ExistsFor[ IsSingletonSubtype, H *: Tail ],
            h : TapirCoproductTranslator.Aux[ T, H, (List[ String ], T => Boolean), Cache ],
            t : TapirCoproductTranslator.Aux[ T, Tail, (List[ String ], T => Boolean), Cache ],
        ) : TapirCoproductTranslator[ T, H *: Tail, Cache ] with {
            type Out = (List[ String ], T => Boolean)

            override def translate(
                shape: H *: Tail,
                cache : Cache,
            ): Out = {
                val (headValues, headMatcher) = h.translate( shape.head, cache )
                val (tailValues, tailMatcher) = t.translate( shape.tail, cache )

                (headValues ++ tailValues, ( t : T ) => headMatcher( t ) || tailMatcher( t ))
            }
        }

        /**
         * Cachelate tuple of mixed singleton and non-singleton subtypes
         */
        given [ T, R <: Tuple, IsSingle <: NonEmptyTuple, NotSingle <: NonEmptyTuple, Cache <: TypeCache ](
            using
            part : Partition.Aux[ IsSingletonSubtype, R, IsSingle, NotSingle ],
            nst : TapirCoproductTranslator.Aux[ T, NotSingle, (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]), Cache ],
            st : TapirCoproductTranslator.Aux[ T, IsSingle, (List[ String ], T => Boolean), Cache ],
        ) : TapirCoproductTranslator[ T, R, Cache ] with {
            type Out = (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ])

            override def translate(
                shape: R,
                cache : Cache,
            ): (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]) = {
                val (isSingle, nonSingle) = part.filter( shape )
                val (singletonValues, singletonMatcher) = st.translate( isSingle, cache )
                val (nonsingleSchema, nonsingleMatcher) = nst.translate( nonSingle, cache )
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
         * Cachelate singleton subtype to string schema + matcher
         */
        given singletonSubtype[ T, ST <: Singleton, D, DN, DV, N <: TypeName, SN <: TypeName, Cache <: TypeCache ] :
          TapirCoproductTranslator[ T, Subtype[ T, ST, D, DN, DV, N, SingletonShape[ ST, SN ] ], Cache ] with {
            type Out = (List[ String ], T => Boolean)

            override def translate(
                shape: Subtype[ T, ST, D, DN, DV, N, SingletonShape[ ST, SN ] ],
                cache : Cache,
            ): (List[ String ], T => Boolean) = {
                (List[ String ]( shape.schema.shape.name ), ( t : T ) => shape.fromSuper( t ).isDefined)
            }
        }

        /**
         * Cachelate non-singleton subtype to tapir schema + matcher
         */
        given nonSingletonSubtype[ T, ST, D, DN, DV, N <: TypeName, S, Cache <: TypeCache ](
          using
          stst : RecursiveSchemaTranslator[ ST, S, Cache, TapirSchema ],
          ev : NotGiven[ IsSingletonSubtype[ Subtype[ T, ST, D, DN, DV, N, S ] ] ],
        ) : TapirCoproductTranslator[ T, Subtype[ T, ST, D, DN, DV, N, S ], Cache ] with {
            type Out = (TapirSchema[ _ ], T => Option[ TapirSchema[ _ ] ])

            override def translate(
                shape: Subtype[ T, ST, D, DN, DV, N, S ],
                cache : Cache,
            ): (TapirSchema[ _ ], T => Option[ TapirSchema[ _ ] ]) = {
                val tSchema = stst.translate( shape.schema, cache )
                val fromSuper = ( v: T ) => shape.fromSuper( v ).map( _ => tSchema )
                (tSchema, fromSuper)
            }
        }

        /**
         * Cachelate lazy subtype to tapir schema + matcher
         */
        given lazySubtype[ T, ST, D, DN, DV, N <: TypeName, Cache <: TypeCache ](
            using
            scr : TypeCacheRetriever.Aux[ Cache, ST, (Option[ String ], String) ],
        ) : TapirCoproductTranslator[ T, LazySubtype[ T, ST, D, DN, DV, N ], Cache ] with {
            type Out = (TapirSchema[ _ ], T => Option[ TapirSchema[ _ ] ])

            override def translate(
                shape: LazySubtype[ T, ST, D, DN, DV, N ],
                cache : Cache,
            ): (TapirSchema[ _ ], T => Option[ TapirSchema[ _ ] ]) = {
                val (nameOpt, refName) = scr.get( cache ).get()
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

    given tapirSchemaCoproductTranslation[ T : ClassTag, R <: NonEmptyTuple, RV <: NonEmptyTuple, D, DN, Cache <: TypeCache ](
        using
        ev : NotGiven[ ExistsFor[ IsSingletonSubtype, R ] ],
        cpt : TapirCoproductTranslator.Aux[ T, R, (List[ TapirSchema[ _ ] ], T => Option[ TapirSchema[ _ ] ]), TypeCache.Cached[Cache, T, (Option[ String ], String)] ],
        dt : TapirDiscriminatorTranslation.Aux[ T, D, DN, R, Option[ SDiscriminator ] ],
        vd : TapirValidatorTranslation[ T ],
    ) : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Cache, TapirSchema ] with {
        type NextCache = TypeCache.Cached[Cache, T, (Option[ String ], String)]

        override def translate(
            schema: Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            cache: Cache,
        ): TapirSchema[ T ] = {
            val classTag = summon[ClassTag[T]]

            lazy val res : TapirSchema[ T ] = {
                val nextCache = cache.add[T]( schema.name, summon[ ClassTag[ T ] ].runtimeClass.getSimpleName ).asInstanceOf[NextCache]
                val (subtypes, fromSuper) = cpt.translate( schema.shape.subtypeDescriptions, nextCache )
                val discr = dt.translate( schema.shape.subtypeDescriptions )

                TapirSchema(
                    schemaType = SCoproduct( subtypes, discr )( (t : T) => fromSuper( t ).map( (s : TapirSchema[_]) => SchemaWithValue[ T ]( s.asInstanceOf[ TapirSchema[ T ] ], t ) ) ),
                    name =Some( TapirSchema.SName( schema.name.getOrElse( classTag.runtimeClass.getSimpleName ) ) ),
                    isOptional = false,
                    description = schema.genericDescription,
                    encodedExample = schema.genericExamples.headOption,
                    deprecated = schema.deprecated,
                    validator = schema.genericValidators
                      .foldLeft( TapirValidator.pass[ T ] )( (validr, nextV ) => validr and vd.translate( nextV ) ),
                    )
            }
            res
        }
    }

    given tapirSchemaSingletonCoproductTranslation[ T : ClassTag, R <: NonEmptyTuple, RV <: NonEmptyTuple, D, DN, Cache <: TypeCache ](
        using
        ev : ExistsFor[ IsSingletonSubtype, R ],
    ) : RecursiveSchemaTranslator[ T, CoproductShape[ T, R, RV, D, DN ], Cache, TapirSchema ] with {
        override def translate(
            schema: Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
            cache : Cache,
        ): TapirSchema[ T ] = {
            val classTag = summon[ClassTag[T]]
            val stList : List[ (T, T => Option[ String ]) ] = schema.shape.subtypeDescriptions.toList map {
                case Subtype( _, ComplexSchema( SingletonShape( caseName, caseValue ), _, _, _, _, _ ), _, matcher, _, _, _, _, _, _, _ ) =>
                    (caseValue.asInstanceOf[ T ], ( t : T ) => matcher.asInstanceOf[ Function1[ T, Option[ Any ] ] ]( t ).map( _ => caseName.asInstanceOf[ String ] ) )
            }

            val encoder : T => Option[ String ] = stList.foldLeft( ( t : T ) => None : Option[ String ] ) { ( lastFn, nextTup ) =>
                val (_, fn) = nextTup
                ( t : T ) => fn( t ) orElse lastFn( t )
            }

            TapirSchema(
                schemaType = SString(),
                name = Some( TapirSchema.SName( schema.name.getOrElse( classTag.runtimeClass.getSimpleName ) ) ),
                isOptional = false,
                description = schema.genericDescription,
                encodedExample = schema.genericExamples.headOption,
                deprecated = schema.deprecated,
                validator = TapirValidator.enumeration( stList.map( _._1 ), encoder ),
            )
        }
    }

    given [ Cache <: TypeCache, T : ClassTag, R <: Tuple, RV <: Tuple, D, DN ] : Cacher[ Cache, T ] with {
        type In = Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ]
        type Out = (Option[ String ], String)

        override def genCachedValue(
            value: Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ]
        ): (Option[ String ], String) = {
            (value.name, summon[ ClassTag[ T ] ].runtimeClass.getSimpleName)
        }

    }

}
