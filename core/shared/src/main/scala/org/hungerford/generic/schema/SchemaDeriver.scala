package org.hungerford.generic.schema

import org.hungerford.generic.schema.Schema.Aux
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

    // For non-tuples
    given plainSchemaDeriver[ T, S ](
        using
        recursiveDeriver : RecursiveSchemaDeriver.Aux[ T, EmptyTuple, S ],
    ) : SchemaDeriver[ T ] with {
        type Shape = S

        override def derive: Schema.Aux[ T, S ] = recursiveDeriver.derive
    }

    def schema[ T ](
        implicit schemaDeriver : SchemaDeriver[ T ],
    ) : Schema.Aux[ T, schemaDeriver.Shape ] = schemaDeriver.derive
}

trait RecursiveSchemaDeriver[ T, Tail <: Tuple ] {
    type Shape

    def derive : Schema.Aux[ T, Shape ]
}


object RecursiveSchemaDeriver {
    type Aux[ T, Tail <: Tuple, S ] = RecursiveSchemaDeriver[ T, Tail ] { type Shape = S }

    def apply[ Ts <: NonEmptyTuple ](
        implicit sd : RecursiveSchemaDeriver[ Tuple.Head[ Ts ], Tuple.Tail[ Ts ] ],
    ) : RecursiveSchemaDeriver.Aux[ Tuple.Head[ Ts ], Tuple.Tail[ Ts ], sd.Shape ] = sd

    given productSchemaDeriver[ T, Tail <: Tuple ](
        using
        prd : ProductDeriver[ T, Tail ],
    ) : RecursiveSchemaDeriver.Aux[ T, Tail, prd.Out ] = new RecursiveSchemaDeriver[ T, Tail ] {
        override type Shape = prd.Out

        override def derive : Schema.Aux[ T, Shape ] = ComplexSchema[ T, Shape ]( prd.derive )
    }

    given coproductSchemaDeriver[ T, Tail <: Tuple ](
        using
        cprd : CoproductDeriver[ T ],
    ) : RecursiveSchemaDeriver.Aux[ T, Tail, cprd.Out ] = new RecursiveSchemaDeriver[ T, Tail ] {
        type Shape = cprd.Out
        def derive : Schema.Aux[ T, Shape ] = ComplexSchema[ T, Shape ]( cprd.derive )
    }

    given singletonSchemaDeriver[ T, Tail <: Tuple ](
        using
        sd : SingletonDeriver[ T ],
    ) : RecursiveSchemaDeriver.Aux[ T, Tail, sd.Out ] = new RecursiveSchemaDeriver[ T, Tail ] {
        type Shape = sd.Out
        def derive : Schema.Aux[ T, Shape ] = ComplexSchema[ T, Shape ]( sd.derive )
    }

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
