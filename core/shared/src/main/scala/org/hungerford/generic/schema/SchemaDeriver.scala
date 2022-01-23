package org.hungerford.generic.schema

import org.hungerford.generic.schema.coproduct.CoproductDeriver
import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.product.{ProductDeriver, ProductShape}
import org.hungerford.generic.schema.singleton.SingletonDeriver
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


    given productSchemaDeriver[ T ](
        using
        prd : ProductDeriver[ T ],
    ) : SchemaDeriver.Aux[ T, prd.Out ] = new SchemaDeriver[ T ] {
        override type Shape = prd.Out

        override def derive : Schema.Aux[ T, Shape ] = ComplexSchema[ T, Shape ]( prd.derive )
    }

    given coproductSchemaDeriver[ T ](
        using
        cprd : CoproductDeriver[ T ],
    ) : SchemaDeriver.Aux[ T, cprd.Out ] = new SchemaDeriver[ T ] {
        type Shape = cprd.Out
        def derive : Schema.Aux[ T, Shape ] = ComplexSchema[ T, Shape ]( cprd.derive )
    }

    given singletonSchemaDeriver[ T ](
        using
        sd : SingletonDeriver[ T ],
    ) : SchemaDeriver.Aux[ T, sd.Out ] = new SchemaDeriver[ T ] {
        type Shape = sd.Out
        def derive : Schema.Aux[ T, Shape ] = ComplexSchema[ T, Shape ]( sd.derive )
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

    given rebuilder[ T ](
        using
        cpd : SchemaDeriver[ T ],
        cprb : SchemaRebuilder[ T, cpd.Shape ],
    ) : SchemaBuildDeriver.Aux[ T, cprb.Builder ] = new SchemaBuildDeriver[ T ] {
        type Builder = cprb.Builder
        def derive : Builder = cprb.rebuild( cpd.derive )
    }

    def builder[ T ](
        implicit schemaBuildDeriver : SchemaBuildDeriver[ T ],
    ) : schemaBuildDeriver.Builder = schemaBuildDeriver.derive

}
