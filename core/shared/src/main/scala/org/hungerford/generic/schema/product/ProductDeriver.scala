package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionCase, FieldNamesCollector}
import org.hungerford.generic.schema.types.Deriver
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{NoSchema, Schema, SchemaProvider, product}
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Tupler

trait ProductDeriver[ T ] extends Deriver[ T ] {
    type Out

    def derive : Out
}

object ProductDeriver {
    type Aux[ T, Out0 ] = ProductDeriver[ T ] { type Out = Out0 }

    def apply[ T ](
        implicit prd : ProductDeriver[ T ],
    ) : ProductDeriver.Aux[ T, prd.Out ] = prd

    implicit def productDeriver[ T, R <: HList, Rt <: HList, RVt <: HList, Tupt ](
        implicit
        lg : LabelledGeneric.Aux[ T, R ],
        gen : Generic.Aux[ T, RVt ],
        fieldDeriver : FieldDeriver.Aux[ R, Rt ],
        lengther : HListIntLength[ Rt ],
        fns : FieldNamesCollector[ Rt ],
        tupler : Tupler.Aux[ RVt, Tupt ],
        valEv : CtxWrapHListsConstraint[ FieldDescription, Rt, RVt ],
    ) : ProductDeriver.Aux[ T, ProductShape[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ] ] = {
        new ProductDeriver[ T ] {
            override type Out = ProductShape[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ]

            override def derive : ProductShape[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ] = {
                ProductShape[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ](
                    fieldDescriptions = fieldDeriver.derive,
                    additionalFieldsSchema = NoSchema,
                    (rv, _) => gen.from( rv ),
                    ( value : T ) => (gen.to( value ), Map.empty),
                )
            }
        }
    }
}

trait FieldDeriver[ T ] extends Deriver[ T ]

object FieldDeriver {
    type Aux[ T, FS ] = FieldDeriver[ T ] { type Out = FS }

    implicit def fieldDescriptionFieldDeriver[ T, K <: Symbol ](
        implicit
        witness: Witness.Aux[ K ],
        provider : SchemaProvider[ T ],
    ) : FieldDeriver.Aux[ FieldType[ K, T ], FieldDescription.Aux[ T, provider.Out ] ] = {
        new FieldDeriver[ FieldType[ K, T ] ] {
            override type Out = FieldDescription.Aux[ T, provider.Out ]

            override def derive : Out = {
                val fn = witness.value.name
                FieldDescriptionCase[ T, provider.Out ]( fn, provider.provide )
            }
        }
    }

    implicit val hnilFDFieldDeriver : FieldDeriver.Aux[ HNil, HNil ] = {
        new FieldDeriver[ HNil ] {
            type Out = HNil

            override def derive : HNil = HNil
        }
    }

    implicit def hlistFDFieldDeriver[ T, S <: Schema[ T ], THead, TTail <: HList, Res <: HList ](
        implicit
        fdFieldDeriver : Lazy[ FieldDeriver.Aux[ THead, FieldDescription.Aux[ T, S ] ] ],
        next : FieldDeriver.Aux[ TTail, Res ],
    ) : FieldDeriver.Aux[ THead :: TTail, FieldDescription.Aux[ T, S ] :: Res ] = {
        new FieldDeriver[ THead :: TTail ] {
            override type Out = FieldDescription.Aux[ T, S ] :: Res

            override def derive : FieldDescription.Aux[ T, S ] :: Res = {
                val headRes = fdFieldDeriver.value.derive
                headRes :: next.derive
            }
        }
    }
}