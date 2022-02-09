package org.hungerford.generic.schema

import org.hungerford.generic.schema.{RecursiveSchemaDeriver, Schema}
import org.hungerford.generic.schema.types.{Provider, Size, Contains, TypeNames}

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonAll, summonInline}
import scala.util.NotGiven

trait SchemaProvider[ T ] {
    type Shape

    def provide : Schema.Aux[ T, Shape ]
}

object SchemaProvider {
    type Aux[ T, S ] = SchemaProvider[ T ] { type Shape = S }

    given recursiveProvider[ T, S ](
        using
        rp : RecursiveSchemaProvider.Aux[ T, EmptyTuple, S ],
    ) : SchemaProvider[ T ] with {
        type Shape = S

        override def provide: Schema.Aux[ T, S ] = rp.provide
    }

    def schema[ T ](
        using sp : RecursiveSchemaProvider[ T, EmptyTuple ],
    ) : Schema.Aux[ T, sp.Shape ] = sp.provide
}

trait RecursiveSchemaProvider[ T, Tail <: Tuple ] {
    type Shape

    def provide : Schema.Aux[ T, Shape ]
}


trait LowestPriorityRecursiveSchemaProviders {

    given emptyPrimitiveProvider[ T, Tail <: Tuple, TLabel <: String, TailLabels <: Tuple ] : RecursiveSchemaProvider.Aux[ T, Tail, Unit ] = {
        new RecursiveSchemaProvider[ T, Tail ] {
            override type Shape = Unit

            override def provide : Schema.Aux[T, Unit] = {
                Primitive()
            }
        }
    }


    given nothingProvider[ Tail <: Tuple ] : RecursiveSchemaProvider.Aux[ Nothing, Tail, Unit ] =
       new RecursiveSchemaProvider[ Nothing, Tail ] {
           override type Shape = Unit

           override def provide : Schema.Aux[ Nothing, Unit ] = NoSchema
       }

}

trait LowPriorityRecursiveSchemaProviders extends LowestPriorityRecursiveSchemaProviders {

   given derivedSchemaProvider[ T, Tail <: Tuple, S ](
       using sd : RecursiveSchemaDeriver[ T, Tail ],
   ) : RecursiveSchemaProvider.Aux[ T, Tail, sd.Shape ] = new RecursiveSchemaProvider[ T, Tail ] {
       override type Shape = sd.Shape

       override def provide : Schema.Aux[T, sd.Shape ] = sd.derive
   }

}

object RecursiveSchemaProvider extends LowPriorityRecursiveSchemaProviders {
    type Aux[ T, Tail <: Tuple, S ] = RecursiveSchemaProvider[ T, Tail ] { type Shape = S }

    given schemaInstanceProvider[ T, Tail <: Tuple ](
        using inst : Schema[ T ],
    ) : RecursiveSchemaProvider.Aux[ T, Tail, inst.Shape ] = new RecursiveSchemaProvider[ T, Tail ] {
        override type Shape = inst.Shape
        override def provide : Schema.Aux[ T, inst.Shape ] = inst
    }
}
