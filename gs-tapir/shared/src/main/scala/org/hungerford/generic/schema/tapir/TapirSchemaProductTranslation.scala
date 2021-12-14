package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, TranslatedFieldDescription, FieldGetter, FieldName}
import org.hungerford.generic.schema.product.translation.FieldBuildingProductSchemaTranslation
import sttp.tapir.Schema as TapirSchema
import sttp.tapir.SchemaType.{SProduct, SProductField}
import sttp.tapir.{FieldName => TapirFieldName}

type TapirFields[ X ] = List[ SProductField[ X ] ]

trait TapirSchemaProductTranslation
  extends FieldBuildingProductSchemaTranslation[ TapirSchema, TapirFields ] {

    override def fieldsInit[ T ] : TapirFields[ T ] = List.empty[ SProductField[ T ] ]

    override def addTranslatedField[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
        field : Field.Aux[ F, N, S ],
        fieldSchema : TapirSchema[ F ],
        to : TapirFields[ T ],
        informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ],
    )(
        using
        fg : FieldGetter.Aux[ N, R, RV, F ],
    ) : TapirFields[ T ] = {
        to :+ SProductField(
            TapirFieldName( field.fieldName.asInstanceOf[ String ] ),
            field.validators
              .foldLeft( fieldSchema )( (sch, nextV ) => sch.validate( TapirValidatorTranslation.translate[ F ]( nextV ) ) ),
            ( v : T ) => Some( informedBy.shape.getField[ N ]( field.fieldName, v ) ),
        )
    }

    override def addAdditionalFields[ T, AF, AFS, R <: Tuple, RV <: Tuple, C, DC ](
        additionalFields : Schema.Aux[ AF, AFS ],
        additionalFieldsTranslated : TapirSchema[ AF ],
        to : TapirFields[ T ],
        informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ],
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

}

object TapirSchemaProductTranslation
  extends TapirSchemaProductTranslation
