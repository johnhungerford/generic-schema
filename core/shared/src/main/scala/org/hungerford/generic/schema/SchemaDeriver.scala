package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.field.{FieldDescription, FieldNamesCollector}
import org.hungerford.generic.schema.product.{CtxWrapHListsConstraint, HListIntLength, ProductDeriver, ProductShape}
import org.hungerford.generic.schema.types.Deriver
import org.hungerford.generic.schema.validator.Validator
import shapeless._
import shapeless.ops.hlist.Tupler

trait SchemaDeriver[ T ] extends Deriver[ T ] {
    type Out <: Schema[ T ]

    def derive : Out
}

object SchemaDeriver {
    type Aux[ T, S <: Schema[ T ] ] = SchemaDeriver[ T ] { type Out = S }

    def apply[ T, S <: Schema[ T ] ](
        implicit sd : SchemaDeriver.Aux[ T, S ],
    ) : SchemaDeriver.Aux[ T, S ] = sd


    implicit def productSchemaDeriver[ T ](
        implicit prd : ProductDeriver[ T ],
    ) : SchemaDeriver.Aux[ T, ComplexSchema[ T, prd.Out ] ] = new SchemaDeriver[ T ] {
        override type Out = ComplexSchema[ T, prd.Out ]

        override def derive : Out = ComplexSchema[ T, prd.Out ]( prd.derive )
    }

    def schema[ T ](
        implicit schemaDeriver : SchemaDeriver[ T ],
    ) : schemaDeriver.Out = schemaDeriver.derive

}
