package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, FieldGetter, FieldName, FieldTranslator, TranslatedFieldDescription}
import org.hungerford.generic.schema.translation.SchemaTranslator

trait FieldBuildingProductSchemaTranslation[ OtherSchema[ _ ], Fields[ _ ] ] {

    def fieldsInit[ T ] : Fields[ T ]

    def addTranslatedField[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
        field : Field.Aux[ F, N, S ],
        fieldSchema : OtherSchema[ F ],
        to : Fields[ T ],
        informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC] ],
    )(
        using
        fg : FieldGetter.Aux[ N, R, RV, F ],
    ) : Fields[ T ]

    def addAdditionalFields[ T, AF, AFS, R <: Tuple, RV <: Tuple, C, DC ](
        additionalFields : Schema.Aux[ AF, AFS ],
        additionalFieldsTranslated : OtherSchema[ AF ],
        to : Fields[ T ],
        informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC] ],
    ) : Fields[ T ]

    def build[ T ]( fields : Fields[ T ], schema : Schema[ T ] ) : OtherSchema[ T ]

    trait FieldBuilder[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ] {
        def addField(
            field : Field.Aux[ F, N, S ],
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC] ],
        ) : Fields[ T ]
    }

    object FieldBuilder {
        given[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ] (
            using
            tr : FieldTranslator[ F, N, S, OtherSchema ],
            fg : FieldGetter.Aux[ N, R, RV, F ],
        ) : FieldBuilder[ T, F, N, S, R, RV, AF, AFS, C, DC ] with {
            override def addField(
                field : Field.Aux[ F, N, S ],
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC] ],
            ) = {
                val fieldSchema = tr.translate( field ).schema
                addTranslatedField[ T, F, N, S, R, RV, AF, AFS, C, DC ]( field, fieldSchema, to, informedBy )
            }
        }
    }

    trait FieldTupleBuilder[ T, FL <: Tuple, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ] {
        def addFields(
            fields : FL,
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC] ],
        ) : Fields[ T ]
    }

    object FieldTupleBuilder {
        given[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ] : FieldTupleBuilder[ T, EmptyTuple, R, RV, AF, AFS, C, DC ] with {
            def addFields(
                fields : EmptyTuple,
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC] ],
            ) : Fields[ T ] = to
        }

        given[ T, F, N <: FieldName, S, Tail <: Tuple, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ] (
            using
            fb : FieldBuilder[ T, F, N, S, R, RV, AF, AFS, C, DC ],
            nb : FieldTupleBuilder[ T, Tail, R, RV, AF, AFS, C, DC ],
        ) : FieldTupleBuilder[ T, Field.Aux[ F, N, S ] *: Tail, R, RV, AF, AFS, C, DC ] with {
            def addFields(
                fields : Field.Aux[ F, N, S ] *: Tail,
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC] ],
            ) : Fields[ T ] = {
                val withThisField = fb.addField( fields.head, to, informedBy )
                nb.addFields( fields.tail, withThisField, informedBy )
            }
        }
    }

    trait AdditionalFieldsBuilder[ T, AF, AFS, R <: Tuple, RV <: Tuple, C, DC ] {
        def addFields(
            additionalFields : Schema.Aux[ AF, AFS ],
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC] ],
        ) : Fields[ T ]
    }

    object AdditionalFieldsBuilder {
        given noAF[ T, R <: Tuple, RV <: Tuple, C, DC ] : AdditionalFieldsBuilder[ T, Nothing, Unit, R, RV, C, DC ] with {
            override def addFields(
                additionalFields : Schema.Aux[ Nothing, Unit ],
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, Nothing, Unit, C, DC] ],
            ) : Fields[ T ] = to
        }

        given[ T, AF, AFS, R <: Tuple, RV <: Tuple, C, DC ] (
            using
            tr : SchemaTranslator[ AF, AFS, OtherSchema ],
        ) : AdditionalFieldsBuilder[ T, AF, AFS, R, RV, C, DC ] with {
            override def addFields(
                additionalFields : Schema.Aux[ AF, AFS ],
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC] ],
            ) : Fields[ T ] = {
                val translatedSchema = tr.translate( additionalFields )
                addAdditionalFields[ T, AF, AFS, R, RV, C, DC ](
                    additionalFields,
                    translatedSchema,
                    to,
                    informedBy,
                )
            }
        }
    }

    given fieldBuildingProductTranslation[ T, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ] (
        using
        ftb : FieldTupleBuilder[ T, R, R, RV, AF, AFS, C, DC ],
        afb : AdditionalFieldsBuilder[ T, AF, AFS, R, RV, C, DC ],
    ) : SchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ], OtherSchema ] with {
        def translate( schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC ] ] ) : OtherSchema[ T ] = {
            val initialFields = fieldsInit[ T ]
            val fieldsWithFieldDescriptions = ftb.addFields( schema.shape.fieldDescriptions, initialFields, schema )
            val allFields = afb.addFields( schema.shape.additionalFieldsSchema, fieldsWithFieldDescriptions, schema )
            build[ T ]( allFields, schema )
        }
    }

}
