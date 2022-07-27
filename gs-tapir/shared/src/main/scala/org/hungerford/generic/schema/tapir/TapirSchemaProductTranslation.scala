package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.coproduct.CoproductShape
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.constructor.ProductDeconstructor
import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.product.field.FieldGetter.Aux
import org.hungerford.generic.schema.product.field.{Field, FieldGetter, FieldName}
import org.hungerford.generic.schema.product.translation.FieldBuildingProductSchemaTranslation
import org.hungerford.generic.schema.translation.{TypeCache, TypeCacheRetriever, CI, Cacher}
import sttp.tapir.Schema as TapirSchema
import sttp.tapir.SchemaType.{SProduct, SProductField}
import sttp.tapir.FieldName as TapirFieldName
import sttp.tapir.SchemaType.SRef
import sttp.tapir.Schema.SName

import scala.util.Try

import scala.reflect.ClassTag

type TapirFields[ X ] = List[ SProductField[ X ] ]

trait TapirSchemaProductTranslation
  extends FieldBuildingProductSchemaTranslation[ TapirSchema, TapirFields ] {

    override def fieldsInit[ T ] : TapirFields[ T ] = List.empty[ SProductField[ T ] ]

    given translatedFieldAdder[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ](
        using
        vt : TapirValidatorTranslation[ F ],
        ev : ProductDeconstructor.Aux[ T, R, RV ],
    ) : FieldAdder[ T, F, N, R, RV, AF, AFS, AFE, C ] with {
            override def addField(
                field: Field.OrLazy[ T, F, N],
                fieldSchema: TapirSchema[ F ],
                to: TapirFields[ T ],
                informedBy: Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            )(
                using
                fg: FieldGetter.Aux[ N, R, RV, F ],
            ): TapirFields[T] = {
                to :+ SProductField(
                    TapirFieldName( field.fieldName.asInstanceOf[ String ] ),
                    field.validators
                      .foldLeft( fieldSchema )( (sch, nextV ) => sch.validate( TapirValidatorTranslation.translate[ F ]( nextV ) ) ),
                    ( v : T ) => Some( informedBy.shape.getField[ N ]( field.fieldName, v ) ),
                )
            }
        }

    given lazyFieldAdder[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Cache <: TypeCache ](
        using
        vt : TapirValidatorTranslation[ F ],
        ev : ProductDeconstructor.Aux[ T, R, RV ],
        fg : FieldGetter.Aux[ N, R, RV, F ],
        rt : TypeCacheRetriever.Aux[ Cache, F, (Option[ String ], String) ],
    ) : LazyFieldAdder[ T, F, N, R, RV, AF, AFS, AFE, C, Cache, (Option[ String ], String) ] with {
        override protected def addField(
            field: Field.OrLazy[ T, F, N ], to: TapirFields[ T ],
            informedBy: Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            cachedItem: CI[ F, (Option[ String ], String) ]
        ): TapirFields[ T ] =
            val (schemaName, refName) = cachedItem.get()
            to :+ SProductField(
                TapirFieldName( field.fieldName ), TapirSchema( SRef( SName( refName ) ), schemaName.map( n => SName( n ) ) ),
                ( v : T ) => Some( informedBy.shape.getField[ N ]( field.fieldName, v ) ),
            )
    }

    // TODO: use type class for this
    override def addAdditionalFields[ T : ClassTag, AF, AFS, AFE, R <: Tuple, RV <: Tuple, C ](
        additionalFields : Schema.Aux[ AF, AFS ],
        additionalFieldsTranslated : TapirSchema[ AF ],
        to : TapirFields[ T ],
        informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
    ) : TapirFields[ T ] = to

    override def build[ T : ClassTag ]( fields : TapirFields[ T ], schema : Schema[ T ] ) : TapirSchema[ T ] = {
        val classTag = summon[ClassTag[T]]
        TapirSchema(
            schemaType = SProduct( fields ),
            name = schema.name.orElse(Try( classTag.runtimeClass.getSimpleName ).toOption).map( n => TapirSchema.SName( n ) ),
            isOptional = false,
            description = schema.genericDescription,
            encodedExample = schema.genericExamples.headOption,
            deprecated = schema.deprecated,
            validator = TapirValidatorTranslation.translateValidators( schema.genericValidators )
        )

    }

    given productCacher[ Cache <: TypeCache, T : ClassTag, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ] : Cacher[ Cache, T ] with {
        type In = Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ]
        type Out = (Option[ String ], String)

        override def genCachedValue(
            value: Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ]
        ): (Option[ String ], String) = {
            (value.name, Try( summon[ ClassTag[ T ] ].runtimeClass.getSimpleName ).getOrElse( value.name.getOrElse( "NAMELESS" ) ) )
        }
    }

}

object TapirSchemaProductTranslation
  extends TapirSchemaProductTranslation
