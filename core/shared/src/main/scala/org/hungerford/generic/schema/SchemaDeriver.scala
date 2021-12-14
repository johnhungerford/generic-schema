package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.product.{TupleIntLength, ProductDeriver, ProductShape, ProductBuildDeriver}
import org.hungerford.generic.schema.types.Deriver
import org.hungerford.generic.schema.validator.Validator


trait SchemaDeriver[ T ] {
    type Shape

    def derive : Schema.Aux[ T, Shape ]
}

object SchemaDeriver {
    type Aux[ T, S ] = SchemaDeriver[ T ] { type Shape = S }

    def apply[ T ](
        implicit sd : SchemaDeriver[ T ],
    ) : SchemaDeriver.Aux[ T, sd.Shape ] = sd


    implicit def productSchemaDeriver[ T ](
        implicit prd : ProductDeriver[ T ],
    ) : SchemaDeriver.Aux[ T, prd.Out ] = new SchemaDeriver[ T ] {
        override type Shape = prd.Out

        override def derive : Schema.Aux[ T, Shape ] = ComplexSchema[ T, Shape ]( prd.derive )
    }

    def schema[ T ](
        implicit schemaDeriver : SchemaDeriver[ T ],
    ) : Schema.Aux[ T, schemaDeriver.Shape ] = schemaDeriver.derive

}

trait SchemaBuildDeriver[ T ] {
    type Builder

    def derive : Builder
}

object SchemaBuildDeriver {
    type Aux[ T, B ] = SchemaBuildDeriver[ T ] { type Builder = B }

    def apply[ T ](
        implicit sd : SchemaBuildDeriver[ T ],
    ) : SchemaBuildDeriver.Aux[ T, sd.Builder ] = sd


    implicit def productSchemaBuildDeriver[ T ](
        implicit prd : ProductBuildDeriver[ T ],
    ) : SchemaBuildDeriver.Aux[ T, prd.Builder ] = new SchemaBuildDeriver[ T ] {
        override type Builder = prd.Builder

        override def derive : Builder = prd.derive
    }

    def builder[ T ](
        implicit schemaBuildDeriver : SchemaBuildDeriver[ T ],
    ) : schemaBuildDeriver.Builder = schemaBuildDeriver.derive

}
