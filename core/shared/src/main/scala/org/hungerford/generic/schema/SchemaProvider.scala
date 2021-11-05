package org.hungerford.generic.schema

import org.hungerford.generic.schema.types.Provider

trait SchemaProvider[ T ] {
    type Out <: Schema[ T ]

    def provide : Out
}

trait LowestPrioritySchemaProviders {

    implicit def emptyPrimitiveProvider[ T ] : SchemaProvider.Aux[ T, Primitive[ T ] ] =
        new SchemaProvider[ T ] {
            override type Out = Primitive[ T ]

            override def provide : Out = Primitive[ T ]()
        }

}

trait LowPrioritySchemaProviders extends LowestPrioritySchemaProviders {

    implicit def derivedSchemaProvider[ T ](
        implicit sd : SchemaDeriver[ T ]
    ) : SchemaProvider.Aux[ T, sd.Out ] = new SchemaProvider[ T ] {
        override type Out = sd.Out

        override def provide : Out = sd.derive
    }

}

object SchemaProvider extends LowPrioritySchemaProviders {
    type Aux[ T, S <: Schema[ T ] ] = SchemaProvider[ T ] { type Out = S }

    def schema[ T ](
        implicit sp : SchemaProvider[ T ],
    ) : sp.Out = sp.provide

    implicit def schemaInstanceProvider[ T ](
        implicit inst : Schema[ T ],
    ) : SchemaProvider.Aux[ T, Schema.Aux[ T, inst.Shape ] ] = new SchemaProvider[ T ] {
        override type Out = Schema.Aux[ T, inst.Shape ]

        override def provide : Out = inst
    }
}
