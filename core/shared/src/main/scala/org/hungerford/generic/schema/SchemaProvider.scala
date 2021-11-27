package org.hungerford.generic.schema

import org.hungerford.generic.schema.Schema.Aux
import org.hungerford.generic.schema.types.Provider

trait SchemaProvider[ T ] {
    type Shape

    def provide : Schema.Aux[ T, Shape ]
}

trait LowestPrioritySchemaProviders {

   given emptyPrimitiveProvider[ T ] : SchemaProvider.Aux[ T, Unit ] =
       new SchemaProvider[ T ] {
           override type Shape = Unit

           override def provide : Schema.Aux[T, Unit] = Primitive()
       }

   given nothingProvider : SchemaProvider.Aux[ Nothing, Unit ] =
       new SchemaProvider[ Nothing ] {
           override type Shape = Unit

           override def provide : Aux[ Nothing, Unit ] = NoSchema
       }

}

trait LowPrioritySchemaProviders extends LowestPrioritySchemaProviders {

   given derivedSchemaProvider[ T ](
       using sd : SchemaDeriver[ T ]
   ) : SchemaProvider.Aux[ T, sd.Shape ] = new SchemaProvider[ T ] {
       override type Shape = sd.Shape

       override def provide : Schema.Aux[T, Shape] = sd.derive
   }

}

object SchemaProvider extends LowPrioritySchemaProviders {
    type Aux[ T, S ] = SchemaProvider[ T ] { type Shape = S }

    given schemaInstanceProvider[ T ](
        using inst : Schema[ T ],
    ) : SchemaProvider.Aux[ T, inst.Shape ] = new SchemaProvider[ T ] {
        override type Shape = inst.Shape
        override def provide : Schema.Aux[ T, inst.Shape ] = inst
    }

    def schema[ T ](
        using sp : SchemaProvider[ T ],
    ) : Schema.Aux[ T, sp.Shape ] = sp.provide
}
