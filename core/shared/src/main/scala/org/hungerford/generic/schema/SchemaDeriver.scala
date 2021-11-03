package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.field.{FieldDescription, FieldNamesCollector}
import org.hungerford.generic.schema.product.{CtxWrapHListsConstraint, HListIntLength, ProductDeriver, ProductSchema}
import org.hungerford.generic.schema.validator.Validator
import shapeless._
import shapeless.ops.hlist.Tupler

trait SchemaDeriver[ T ] {
    type Out <: Schema[ T ]

    def derive : Out
}

object SchemaDeriver {
    type Aux[ T, S <: Schema[ T ] ] = SchemaDeriver[ T ] { type Out = S }

    def apply[ T, S <: Schema[ T ] ](
        implicit sd : SchemaDeriver.Aux[ T, S ],
    ) : SchemaDeriver.Aux[ T, S ] = sd

    implicit def productDeriver[ T, R <: HList, Rt <: HList, RVt <: HList, Tupt ](
        implicit
        pd : ProductDeriver.Aux[ T, ProductSchema[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ] ],

    ) : SchemaDeriver.Aux[ T, ProductSchema[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ] ] = {
        new SchemaDeriver[ T ] {
            override type Out = pd.Out

            override def derive : ProductSchema[ T, Rt, RVt, Nothing, NoSchema.type, Tupt ] = {
                pd.derive
            }
        }
    }

    def schema[ T ](
        implicit schemaDeriver : SchemaDeriver[ T ],
    ) : schemaDeriver.Out = schemaDeriver.derive

}
