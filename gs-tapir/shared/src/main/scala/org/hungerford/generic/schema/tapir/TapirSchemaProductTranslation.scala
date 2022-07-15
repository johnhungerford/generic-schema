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
import org.hungerford.generic.schema.translation.{CI, Cached, SchemaCacheRetriever}
import sttp.tapir.Schema as TapirSchema
import sttp.tapir.SchemaType.{SProduct, SProductField}
import sttp.tapir.FieldName as TapirFieldName
import sttp.tapir.SchemaType.SRef
import sttp.tapir.Schema.SName

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

    given lazyFieldAdder[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ](
        using
        vt : TapirValidatorTranslation[ F ],
        ev : ProductDeconstructor.Aux[ T, R, RV ],
    ) : LazyFieldAdder[ T, F, N, R, RV, AF, AFS, AFE, C, String ] with {
        override def addField(
            field: Field.OrLazy[ T, F, N],
            cacheItem : CI[ F, String ],
            to: TapirFields[ T ],
            informedBy: Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
        )(
            using
            fg: FieldGetter.Aux[ N, R, RV, F ],
        ): TapirFields[T] = {
            to :+ SProductField(
                TapirFieldName( field.fieldName.asInstanceOf[ String ] ), TapirSchema( SRef( SName( cacheItem.get() ) ) ),
                ( v : T ) => Some( informedBy.shape.getField[ N ]( field.fieldName, v ) ),
            )
        }
    }

    // TODO: use type class for this
    override def addAdditionalFields[ T, AF, AFS, AFE, R <: Tuple, RV <: Tuple, C ](
        additionalFields : Schema.Aux[ AF, AFS ],
        additionalFieldsTranslated : TapirSchema[ AF ],
        to : TapirFields[ T ],
        informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
    ) : TapirFields[ T ] = to

    override def build[ T ]( fields : TapirFields[ T ], schema : Schema[ T ] ) : TapirSchema[ T ] = {
        TapirSchema(
            SProduct( fields ),
            schema.name.map( n => TapirSchema.SName( n ) ),
            false,
            schema.genericDescription,
            None,
            None,
            schema.genericExamples.headOption,
            schema.deprecated,
            TapirValidatorTranslation.translateValidators( schema.genericValidators )
        )

    }

    given productCached[ Trans <: Tuple, T : ClassTag, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ] : Cached[ Trans, T ] with {
        type In = Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ]
        type Out = String

        override def cacheItem(
            value: Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ]
        ): String = {
            value.name getOrElse {
                summon[ ClassTag[ T ] ].runtimeClass.getSimpleName
            }
        }
    }

}

object TapirSchemaProductTranslation
  extends TapirSchemaProductTranslation
