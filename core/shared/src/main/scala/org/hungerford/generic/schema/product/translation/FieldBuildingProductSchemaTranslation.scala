package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, FieldGetter, FieldName}
import org.hungerford.generic.schema.translation.SchemaTranslator

trait FieldBuildingProductSchemaTranslation[ OtherSchema[ _ ], Fields[ _ ] ] {

    def fieldsInit[ T ] : Fields[ T ]

    trait TranslatedFieldAdder[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ] {
        def addTranslatedField(
          field : Field.Aux[ T, F, N, S ],
          fieldSchema : OtherSchema[ F ],
          to : Fields[ T ],
          informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
        )(
          using
          fg : FieldGetter.Aux[ N, R, RV, F ],
        ) : Fields[ T ]
    }

//    def addTranslatedField[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
//        field : Field.Aux[ F, N, S ],
//        fieldSchema : OtherSchema[ F ],
//        to : Fields[ T ],
//        informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, C, DC] ],
//    )(
//        using
//        fg : FieldGetter.Aux[ N, R, RV, F ],
//    ) : Fields[ T ]

    def addAdditionalFields[ T, AF, AFS, AFE, R <: Tuple, RV <: Tuple, C ](
        additionalFields : Schema.Aux[ AF, AFS ],
        additionalFieldsTranslated : OtherSchema[ AF ],
        to : Fields[ T ],
        informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
    ) : Fields[ T ]

    def build[ T ]( fields : Fields[ T ], schema : Schema[ T ] ) : OtherSchema[ T ]

    trait FieldBuilder[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ] {
        def addField(
            field : Field.Aux[ T, F, N, S ],
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
        ) : Fields[ T ]
    }

    object FieldBuilder {
        given[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ] (
            using
            fa : TranslatedFieldAdder[ T, F, N, S, R, RV, AF, AFS, AFE, C ],
            sr : SchemaTranslator[ F, S, OtherSchema ],
            fg : FieldGetter.Aux[ N, R, RV, F ],
        ) : FieldBuilder[ T, F, N, S, R, RV, AF, AFS, AFE, C ] with {
            override def addField(
                field : Field.Aux[ T, F, N, S ],
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            ) = {
                val fieldSchema = sr.translate( field.schema )
                fa.addTranslatedField( field, fieldSchema, to, informedBy )
            }
        }
    }

    trait FieldTupleBuilder[ T, FL <: Tuple, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ] {
        def addFields(
            fields : FL,
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
        ) : Fields[ T ]
    }

    object FieldTupleBuilder {
        given[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ] : FieldTupleBuilder[ T, EmptyTuple, R, RV, AF, AFS, AFE, C ] with {
            def addFields(
                fields : EmptyTuple,
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            ) : Fields[ T ] = to
        }

        given[ T, F, N <: FieldName, S, Tail <: Tuple, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ] (
            using
            fb : FieldBuilder[ T, F, N, S, R, RV, AF, AFS, AFE, C ],
            nb : FieldTupleBuilder[ T, Tail, R, RV, AF, AFS, AFE, C ],
        ) : FieldTupleBuilder[ T, Field.Aux[ T, F, N, S ] *: Tail, R, RV, AF, AFS, AFE, C ] with {
            def addFields(
                fields : Field.Aux[ T, F, N, S ] *: Tail,
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            ) : Fields[ T ] = {
                val withThisField = fb.addField( fields.head, to, informedBy )
                nb.addFields( fields.tail, withThisField, informedBy )
            }
        }
    }

    trait AdditionalFieldsBuilder[ T, AF, AFS, AFE, R <: Tuple, RV <: Tuple, C ] {
        def addFields(
            additionalFields : Schema.Aux[ AF, AFS ],
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
        ) : Fields[ T ]
    }

    object AdditionalFieldsBuilder {
        given noAF[ T, R <: Tuple, RV <: Tuple, C ] : AdditionalFieldsBuilder[ T, Nothing, Unit, Unit, R, RV, C ] with {
            override def addFields(
                additionalFields : Schema.Aux[ Nothing, Unit ],
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, Nothing, Unit, Unit, C ] ],
            ) : Fields[ T ] = to
        }

        given[ T, AF, AFS, AFE, R <: Tuple, RV <: Tuple, C ] (
            using
            tr : SchemaTranslator[ AF, AFS, OtherSchema ],
        ) : AdditionalFieldsBuilder[ T, AF, AFS, AFE, R, RV, C ] with {
            override def addFields(
                additionalFields : Schema.Aux[ AF, AFS ],
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            ) : Fields[ T ] = {
                val translatedSchema = tr.translate( additionalFields )
                addAdditionalFields[ T, AF, AFS, AFE, R, RV, C ](
                    additionalFields,
                    translatedSchema,
                    to,
                    informedBy,
                )
            }
        }
    }

    given fieldBuildingProductTranslation[ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ] (
        using
        ftb : FieldTupleBuilder[ T, R, R, RV, AF, AFS, AFE, C ],
        afb : AdditionalFieldsBuilder[ T, AF, AFS, AFE, R, RV, C ],
    ) : SchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], OtherSchema ] with {
        def translate( schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ] ) : OtherSchema[ T ] = {
            val initialFields = fieldsInit[ T ]
            val fieldsWithFieldDescriptions = ftb.addFields( schema.shape.fieldDescriptions, initialFields, schema )
            val allFields = afb.addFields( schema.shape.additionalFieldsSchema, fieldsWithFieldDescriptions, schema )
            build[ T ]( allFields, schema )
        }
    }

}
