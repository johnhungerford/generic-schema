package org.hungerford.generic.schema.product.translation

import org.hungerford.generic.schema.{Schema, translation}
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.{Field, FieldGetter, FieldName, LazyField}
import org.hungerford.generic.schema.translation.{CI, Cacher, RecursiveSchemaTranslator, SchemaTranslator, TypeCache, TypeCacheRetriever}

import scala.reflect.ClassTag

trait FieldBuildingProductSchemaTranslation[ OtherSchema[ _ ], Fields[ _ ] ] {

    def fieldsInit[ T ] : Fields[ T ]

    trait LazyFieldAdder[ T, F, N <: FieldName, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Cache <: TypeCache, O ](
        using
        fg : FieldGetter.Aux[ N, R, RV, F ],
        rt : TypeCacheRetriever.Aux[ Cache, F, O ],
    ) {
        final def addFieldWithTrans(
            field : Field.OrLazy[ T, F, N ],
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            cache : Cache,
        ) : Fields[ T ] =
            addField( field, to, informedBy, rt.get( cache ) )

        protected def addField(
            field : Field.OrLazy[ T, F, N ],
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            cachedItem : CI[ F, O ],
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

    def addAdditionalFields[ T : ClassTag, AF, AFS, AFE, R <: Tuple, RV <: Tuple, C ](
        additionalFields : Schema.Aux[ AF, AFS ],
        additionalFieldsTranslated : OtherSchema[ AF ],
        to : Fields[ T ],
        informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
    ) : Fields[ T ]

    def build[ T : ClassTag ]( fields : Fields[ T ], schema : Schema[ T ] ) : OtherSchema[ T ]

    trait FieldTupleBuilder[ T, FL <: Tuple, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Cache <: TypeCache ] {
        def addFields(
            fields : FL,
            to : Fields[ T ],
            informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
            cache : Cache,
        ) : Fields[ T ]
    }

    object FieldTupleBuilder {
        given [ T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Cache <: TypeCache ] : FieldTupleBuilder[ T, EmptyTuple, R, RV, AF, AFS, AFE, C, Cache ] with {
            def addFields(
                fields : EmptyTuple,
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
                cache : Cache,
            ) : Fields[ T ] = to
        }

        given nonLazyField[ T, F, N <: FieldName, S, Tail <: Tuple, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Cache <: TypeCache ] (
            using
            ft : RecursiveSchemaTranslator[ F, S, Cache, OtherSchema ],
            fa : FieldAdder[ T, F, N, R, RV, AF, AFS, AFE, C ],
            fg : FieldGetter.Aux[ N, R, RV, F ],
            nb : FieldTupleBuilder[ T, Tail, R, RV, AF, AFS, AFE, C, Cache ],
        ) : FieldTupleBuilder[ T, Field[ T, F, N, S ] *: Tail, R, RV, AF, AFS, AFE, C, Cache ] with {
            def addFields(
                fields : Field[ T, F, N, S ] *: Tail,
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
                cache : Cache,
            ) : Fields[ T ] = {
                val field = fields.head
                val fieldSchema = field.schema
                val translatedFieldSchema = ft.translate( fieldSchema, cache )
                val updatedFields = fa.addField( field, translatedFieldSchema, to, informedBy )
                nb.addFields( fields.tail, updatedFields, informedBy, cache )
            }

        }

        given lazyField[ T, F, N <: FieldName, Tail <: Tuple, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Cache <: TypeCache, O ] (
            using
            fa : LazyFieldAdder[ T, F, N, R, RV, AF, AFS, AFE, C, Cache, O ],
            fg : FieldGetter.Aux[ N, R, RV, F ],
            nb : FieldTupleBuilder[ T, Tail, R, RV, AF, AFS, AFE, C, Cache ],
        ) : FieldTupleBuilder[ T, LazyField[ T, F, N ] *: Tail, R, RV, AF, AFS, AFE, C, Cache ] with {
            def addFields(
                fields : LazyField[ T, F, N ] *: Tail,
                to : Fields[ T ],
                informedBy : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
                cache : Cache,
            ) : Fields[ T ] = {
                val field = fields.head
                val updatedFields = fa.addFieldWithTrans( field, to, informedBy, cache )
                nb.addFields( fields.tail, updatedFields, informedBy, cache )
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

        given[ T : ClassTag, AF, AFS, AFE, R <: Tuple, RV <: Tuple, C ] (
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

    given fieldBuildingProductTranslation[ T : ClassTag, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, Cache <: TypeCache, NewCache <: TypeCache ] (
        using
        c : Cacher.AuxINT[ Cache, T, Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ], NewCache ],
        ftb : FieldTupleBuilder[ T, R, R, RV, AF, AFS, AFE, C, NewCache ],
        afb : AdditionalFieldsBuilder[ T, AF, AFS, AFE, R, RV, C ],
    ) : RecursiveSchemaTranslator[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ], Cache, OtherSchema ] with {
        def translate( schema : Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ], cache : Cache ) : OtherSchema[ T ] = {
            lazy val res : OtherSchema[ T ] = {
                val cachedCache = c.cached( schema, cache )
                val initialFields = fieldsInit[ T ]
                val fieldsWithFieldDescriptions = ftb.addFields( schema.shape.fieldDescriptions, initialFields, schema, cachedCache )
                val allFields = afb.addFields( schema.shape.additionalFieldsSchema, fieldsWithFieldDescriptions, schema )
                build[ T ]( allFields, schema )
            }
            res
        }
    }

}
