package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{FieldTranslator, TranslatedFieldDescription, FieldName, FieldDescription}
import org.hungerford.generic.schema.translation.SchemaTranslator

trait FieldBuildingProductSchemaTranslation[ OtherSchema[ _ ], Fields[ _ ] ] {

    def fieldsInit[ T ] : Fields[ T ]

    def addTranslatedField[ T, F ](
        field : TranslatedFieldDescription[ F, OtherSchema ],
        to : Fields[ T ],
    ) : Fields[ T ]

    def addAdditionalFields[ T, AF ](
        additionalFields : Schema[ AF ],
        additionalFieldsTranslated : OtherSchema[ AF ],
        to : Fields[ T ],
    ) : Fields[ T ]

    def build[ T ]( fields : Fields[ T ], schema : Schema[ T ] ) : OtherSchema[ T ]

    trait FieldBuilder[ T, F, N <: FieldName, S ] {
        def addField(
            field : FieldDescription.Aux[ F, N, S ],
            to : Fields[ T ],
        ) : Fields[ T ]
    }

    object FieldBuilder {
        given[ T, F, N <: FieldName, S ] (
            using
            tr : FieldTranslator[ F, N, S, OtherSchema ],
        ) : FieldBuilder[ T, F, N, S ] with {
            override def addField(
                field : FieldDescription.Aux[ F, N, S ],
                to : Fields[ T ],
            ) = {
                val translatedFieldDescription = tr.translate( field )
                addTranslatedField[ T, F ]( translatedFieldDescription, to )
            }
        }
    }

    trait FieldTupleBuilder[ T, R <: Tuple ] {
        def addFields(
            fields : R,
            to : Fields[ T ],
        ) : Fields[ T ]
    }

    object FieldTupleBuilder {
        given[ T ] : FieldTupleBuilder[ T, EmptyTuple ] with {
            def addFields( fields : EmptyTuple, to : Fields[ T ] ) : Fields[ T ] = to
        }

        given[ T, F, N <: FieldName, S, Tail <: Tuple ] (
            using
            fb : FieldBuilder[ T, F, N, S ],
            nb : FieldTupleBuilder[ T, Tail ],
        ) : FieldTupleBuilder[ T, FieldDescription.Aux[ F, N, S ] *: Tail ] with {
            def addFields( fields : FieldDescription.Aux[ F, N, S ] *: Tail, to : Fields[ T ] ) : Fields[ T ] = {
                val withThisField = fb.addField( fields.head, to )
                nb.addFields( fields.tail, withThisField )
            }
        }
    }

    trait AdditionalFieldsBuilder[ T, AF, S ] {
        def addFields( additionalFields : Schema.Aux[ AF, S ], to : Fields[ T ] ) : Fields[ T ]
    }

    object AdditionalFieldsBuilder {
        given noAF[ T ] : AdditionalFieldsBuilder[ T, Nothing, Unit ] with {
            override def addFields( additionalFields : Schema.Aux[ Nothing, Unit ], to : Fields[ T ] ) : Fields[ T ] = to
        }

        given[ T, AF, S ] (
            using
            tr : SchemaTranslator[ AF, S, OtherSchema ],
        ) : AdditionalFieldsBuilder[ T, AF, S ] with {
            override def addFields( additionalFields : Schema.Aux[ AF, S ], to : Fields[ T ] ) : Fields[ T ] = {
                val translatedSchema = tr.translate( additionalFields )
                addAdditionalFields[ T, AF ]( additionalFields, translatedSchema, to )
            }
        }
    }

    given fieldBuildingProductTranslation[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ] (
        using
        ftb : FieldTupleBuilder[ T, R ],
        afb : AdditionalFieldsBuilder[ T, AF, AFS ],
    ) : SchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ], OtherSchema ] with {
        def translate( schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ] ) : OtherSchema[ T ] = {
            val initialFields = fieldsInit[ T ]
            val fieldsWithFieldDescriptions = ftb.addFields( schema.shape.fieldDescriptions, initialFields )
            val allFields = afb.addFields( schema.shape.additionalFieldsSchema, fieldsWithFieldDescriptions )
            build[ T ]( allFields, schema )
        }
    }

}
