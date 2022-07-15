package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.Schema
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, FieldGetter, FieldName, LazyField}
import org.hungerford.generic.schema.translation.{CI, Cached, RecursiveSchemaTranslator, SchemaCacheRetriever, SchemaTranslator}

import scala.reflect.ClassTag

trait FieldBuildingProductSchemaTranslation[ OtherSchema[ _ ], Fields[ _ ] ] {

    def fieldsInit[ T ] : Fields[ T ]

    trait LazyFieldAdder[ T, F, N <: FieldName, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, O ] {
        def addField(
            field : Field.OrLazy[ T, F, N ],
            cacheItem : CI[ F, O ],
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
        )(
            using
            fg : FieldGetter.Aux[ N, R, RV, F ],
        ) : Fields[ T ]
    }

    trait FieldAdder[ T, F, N <: FieldName, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C ] {
        def addField(
            field : Field.OrLazy[ T, F, N ],
            fieldSchema : OtherSchema[ F ],
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
        )(
            using
            fg : FieldGetter.Aux[ N, R, RV, F ],
        ) : Fields[ T ]
    }

    //    def addTranslatedField[ T, F, N <: FieldName, S, R <: Tuple, RV <: Tuple, AF, AFS, C, DC ](
    //        field : Field[ F, N, S ],
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

    trait FieldTupleBuilder[ T, FL <: Tuple, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Trans <: Tuple ] {
        def addFields(
            fields : FL,
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            trans : Trans,
        ) : Fields[ T ]
    }

    object FieldTupleBuilder {
        given [ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Trans <: Tuple ] : FieldTupleBuilder[ T, EmptyTuple, R, RV, AF, AFS, AFE, C, Trans ] with {
            def addFields(
                fields : EmptyTuple,
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
                trans : Trans,
            ) : Fields[ T ] = to
        }

        given nonLazyField[ T, F, N <: FieldName, S, Tail <: Tuple, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Trans <: Tuple ] (
            using
            ft : RecursiveSchemaTranslator[ F, S, Trans, OtherSchema ],
            fa : FieldAdder[ T, F, N, R, RV, AF, AFS, AFE, C ],
            fg : FieldGetter.Aux[ N, R, RV, F ],
            nb : FieldTupleBuilder[ T, Tail, R, RV, AF, AFS, AFE, C, Trans ],
        ) : FieldTupleBuilder[ T, Field[ T, F, N, S ] *: Tail, R, RV, AF, AFS, AFE, C, Trans ] with {
            def addFields(
                fields : Field[ T, F, N, S ] *: Tail,
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
                trans : Trans,
            ) : Fields[ T ] = {
                val field = fields.head
                val fieldSchema = field.schema
                val translatedFieldSchema = ft.translate( fieldSchema, trans )
                val updatedFields = fa.addField( field, translatedFieldSchema, to, informedBy )
                nb.addFields( fields.tail, updatedFields, informedBy, trans )
            }

        }

        given lazyField[ T, F, N <: FieldName, Tail <: Tuple, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Trans <: Tuple, O ] (
            using
            fg : FieldGetter.Aux[ N, R, RV, F ],
            rt : SchemaCacheRetriever.Aux[ Trans, F, O ],
            fa : LazyFieldAdder[ T, F, N, R, RV, AF, AFS, AFE, C, O ],
            nb : FieldTupleBuilder[ T, Tail, R, RV, AF, AFS, AFE, C, Trans ],
        ) : FieldTupleBuilder[ T, LazyField[ T, F, N ] *: Tail, R, RV, AF, AFS, AFE, C, Trans ] with {
            def addFields(
                fields : LazyField[ T, F, N ] *: Tail,
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
                trans : Trans,
            ) : Fields[ T ] = {
                val field = fields.head
                val cachedItem = rt.getter( trans )
                val updatedFields = fa.addField( field, cachedItem, to, informedBy )
                nb.addFields( fields.tail, updatedFields, informedBy, trans )
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

    given fieldBuildingProductTranslation[ T : ClassTag, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Trans <: Tuple, NewTrans <: Tuple ] (
        using
        c : Cached.AuxINT[ Trans, T, Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ], NewTrans ],
        ftb : FieldTupleBuilder[ T, R, R, RV, AF, AFS, AFE, C, NewTrans ],
        afb : AdditionalFieldsBuilder[ T, AF, AFS, AFE, R, RV, C ],
    ) : RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Trans, OtherSchema ] with {
        def translate( schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ], trans : Trans ) : OtherSchema[ T ] = {
            lazy val res : OtherSchema[ T ] = {
                val cachedTrans = c.cached( schema, trans )
                val initialFields = fieldsInit[ T ]
                val fieldsWithFieldDescriptions = ftb.addFields( schema.shape.fieldDescriptions, initialFields, schema, cachedTrans )
                val allFields = afb.addFields( schema.shape.additionalFieldsSchema, fieldsWithFieldDescriptions, schema )
                build[ T ]( allFields, schema )
            }
            res
        }
    }

}
