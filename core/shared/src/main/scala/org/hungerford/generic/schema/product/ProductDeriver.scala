package org.hungerford.generic.schema.product

import org.hungerford.generic.schema
import org.hungerford.generic.schema.product.field.FieldDescription.Aux
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionCase, FieldNamesCollector}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{NoSchema, Schema, SchemaDeriver, product}
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Tupler

trait ProductDeriver[ T ] {
    type Out

    def derive : Out
}

object ProductDeriver {
    type Aux[ T, Out0 ] = ProductDeriver[ T ] { type Out = Out0 }

    implicit def fieldDescriptionDeriver[ T, S <: Schema[ T ], K <: Symbol ](
        implicit
        witness: Witness.Aux[ K ],
        schema : S,
    ) : ProductDeriver.Aux[ FieldType[ K, T ], FieldDescription.Aux[ T, S ] ] = {
        new ProductDeriver[ FieldType[ K, T ] ] {
            override type Out = FieldDescription.Aux[ T, S ]

            override def derive : Out = {
                val fn = witness.value.name
                FieldDescriptionCase[ T, S ]( fn, schema )
            }
        }
    }

    implicit val hnilFDDeriver : product.ProductDeriver.Aux[ HNil, HNil ] = {
        new ProductDeriver[ HNil ] {
            type Out = HNil

            override def derive : HNil = HNil
        }
    }

    implicit def hlistFDDeriver[ T, S <: Schema[ T ], THead, TTail <: HList, Res <: HList ](
        implicit
        fdDeriver : Lazy[ ProductDeriver.Aux[ THead, FieldDescription.Aux[ T, S ] ] ],
        next : ProductDeriver.Aux[ TTail, Res ],
    ) : ProductDeriver.Aux[ THead :: TTail, FieldDescription.Aux[ T, S ] :: Res ] = {
        new ProductDeriver[ THead :: TTail ] {
            override type Out = FieldDescription.Aux[ T, S ] :: Res

            override def derive : FieldDescription.Aux[ T, S ] :: Res = {
                val headRes = fdDeriver.value.derive
                headRes :: next.derive
            }
        }
    }


    implicit def productDeriver[ T, R <: HList, Rt <: HList, RVt <: HList, Tupt ](
        implicit
        lg : LabelledGeneric.Aux[ T, R ],
        fieldDeriver : ProductDeriver.Aux[ R, Rt ],
        gen : Generic.Aux[ T, RVt ],
        valEv : CtxWrapHListsConstraint[ FieldDescription, Rt, RVt ],
        lengther : HListIntLength[ Rt ],
        fns : FieldNamesCollector[ Rt ],
        tupler : Tupler.Aux[ RVt, Tupt ],
    ) : ProductDeriver.Aux[ T, ProductSchema[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ] ] = {
        new ProductDeriver[ T ] {
            override type Out = ProductSchema[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ]

            override def derive : ProductSchema[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ] = {
                ProductSchema[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ](
                    genericDescription = None,
                    genericValidators = Set.empty[ Validator[ T ] ],
                    fieldDescriptions = fieldDeriver.derive,
                    additionalFieldsSchema = NoSchema,
                    (rv, _) => gen.from( rv ),
                    ( value : T ) => (gen.to( value ), Map.empty),
                )
            }
        }
    }
}
