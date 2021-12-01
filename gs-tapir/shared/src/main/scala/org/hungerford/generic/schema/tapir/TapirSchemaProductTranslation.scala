package org.hungerford.generic.schema.tapir

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.field.TranslatedFieldDescription
import org.hungerford.generic.schema.product.translation.FieldBuildingProductSchemaTranslation
import sttp.tapir.{Schema => TapirSchema}
import sttp.tapir.SchemaType.{SProductField, SProduct}
import sttp.tapir.FieldName

type TapirFields[ X ] = List[ SProductField[ X ] ]

trait TapirSchemaProductTranslation
  extends FieldBuildingProductSchemaTranslation[ TapirSchema, TapirFields ] {

    override def fieldsInit[ T ] : TapirFields[ T ] = List.empty[ SProductField[ T ] ]

    override def addTranslatedField[ T, F ](
        field : TranslatedFieldDescription[ F, TapirSchema ],
        to : TapirFields[ T ],
    ) : TapirFields[ T ] = {
        to :+ SProductField( FieldName( field.fieldName ), field.schema, _ => None )
    }

    override def addAdditionalFields[ T, AF ](
        additionalFields : Schema[ AF ],
        additionalFieldsTranslated : TapirSchema[ AF ],
        to : TapirFields[ T ],
    ) : TapirFields[ T ] = to

    override def build[ T ]( fields : TapirFields[ T ], schema : Schema[ T ] ) : TapirSchema[ T ] = {
        TapirSchema(
            SProduct( fields ),
            None,
            false,
            schema.genericDescription,
            None,
            None,
            None,
        )

    }

}

object TapirSchemaProductTranslation
  extends TapirSchemaProductTranslation
