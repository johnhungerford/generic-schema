package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionCase}
import org.hungerford.generic.schema.{NoSchema, Schema}
import shapeless._
import shapeless.ops.hlist.Tupler

trait ProductDeriver[ T ] {
    type S <: Schema[ T ]

    def derive : S
}

object ProductDeriver {
    type Aux[ T, St <: Schema[ T ] ] = ProductDeriver[ T ] { type S = St }

    def apply[ T, R <: HList, Rt <: HList, RVt <: HList, Tupt ](
        implicit
        lg : LabelledGeneric.Aux[ T, R ],
        valEv : CtxWrapHListsConstraint[ FieldDescription, R, Rt ],
        tupler : Tupler.Aux[ RVt, Tupt ],
    ) : ProductDeriver.Aux[ T, ProductSchema[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ] ] = {
        new ProductDeriver[ T ] {
            override type S = ProductSchema[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ]

            override def derive : ProductSchema[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ] = {

            }
        }
    }
}

trait FieldDescriptionDeriver[ T, S <: Schema[ T ] ] {
    def derive( t : T ) : FieldDescription.Aux[ T, S ]
}

object FieldDescriptionDeriver {
    implicit def fieldTypeDeriver[ T, K <: Symbol, S <: Schema[ T ] ](
        implicit
        witness: Witness.Aux[ K ],
        schema : S,
    ) : FieldDescription.Aux[ T, S ] = {
        val fn = witness.value.name
        FieldDescriptionCase[ T, S ]( fn, schema )
    }
}

trait FDLDeriver[ TL <: HList, FDL <: HList ] {
    def derive( t : TL ) : FDL
}

object FDLDeriver {
    implicit def
}


