package org.hungerford.generic.schema

import org.hungerford.generic.schema.coproduct.{CoproductSchemaExtractor, CoproductShape}
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, TypeName}
import org.hungerford.generic.schema.product.{ProductSchemaExtractor, ProductShape}
import org.hungerford.generic.schema.product.field.Field
import org.hungerford.generic.schema.{RecursiveSchemaDeriver, Schema}
import org.hungerford.generic.schema.types.{Contains, Provider, Size, TypeNames}

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonAll, summonInline}
import scala.util.NotGiven

trait SchemaExtractor[ T, From ] {
    type Shape

    def extract( from : From ) : Schema.Aux[ T, Shape ]
}

object SchemaExtractor {
    type Aux[ T, From, S ] = SchemaExtractor[ T, From ] { type Shape = S }

    given productSchemaExtractor[ F, T, R <: Tuple, RV <: Tuple, AF, AFS, AFE, C, S ](
        using
        prExtr : ProductSchemaExtractor.Aux[ F, ProductShape[ T, R, RV, AF, AFS, AFE, C ], S ]
    ) : SchemaExtractor[ F, Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ] ] with {
        type Shape = S

        override def extract(
            from: Schema.Aux[ T, ProductShape[ T, R, RV, AF, AFS, AFE, C ] ],
        ): Schema.Aux[ F, S ] = prExtr.extract( from.shape )
    }

    given coproductSchemaExtractor[ F, T, ST, R <: Tuple, RV <: Tuple, D, DN, S ](
        using
        coprExtr : CoproductSchemaExtractor.Aux[ F, CoproductShape[ T, R, RV, D, DN ], S ],
    ) : SchemaExtractor[ F, Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ] ] with {
        type Shape = S

        override def extract(
            from: Schema.Aux[ T, CoproductShape[ T, R, RV, D, DN ] ],
        ): Schema.Aux[ F, S ] = coprExtr.extract( from.shape )
    }

    given fieldSchemaExtractor[ T, F, FS, Fld <: Field.Shaped[ F, FS ], S ](
        using
        extr : SchemaExtractor.Aux[ T, Schema.Aux[ F, FS ], S ],
    ) : SchemaExtractor[ T, Fld ] with {
        type Shape = S

        override def extract(
            from: Fld
        ): Schema.Aux[ T, S ] = extr.extract( from.schema )
    }

    given subtypeSchemaExtractor[ T, STT, ST, STS, STD, STDN, STDV, STN <: TypeName, S ](
        using
        extr : SchemaExtractor.Aux[ T, Schema.Aux[ ST, STS ], S ],
    ) : SchemaExtractor[ T, Subtype.Aux[ STT, ST, STD, STDN, STDV, STN, STS ] ] with {
        type Shape = S

        override def extract(
            from: Subtype.Aux[ STT, ST, STD, STDN, STDV, STN, STS ],
        ): Schema.Aux[ T, S ] = extr.extract( from.schema )
    }

}

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


trait RecursiveSchemaProvidersPriority3 {

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

trait RecursiveSchemaProvidersPriority2 extends RecursiveSchemaProvidersPriority3 {

   given derivedSchemaProvider[ T, Tail <: Tuple, S ](
       using sd : RecursiveSchemaDeriver[ T, Tail ],
   ) : RecursiveSchemaProvider.Aux[ T, Tail, sd.Shape ] = new RecursiveSchemaProvider[ T, Tail ] {
       override type Shape = sd.Shape

       override def provide : Schema.Aux[T, sd.Shape ] = sd.derive
   }

}

trait RecursiveSchemaProvidersPriority1 extends RecursiveSchemaProvidersPriority2 {
    given extractedSchemaProvider[ T, OuterT, OuterS, Sch <: Schema.Aux[ OuterT, OuterS ], S, Tail <: Tuple ](
        using
        inst : Sch,
        extr : SchemaExtractor.Aux[ T, Sch, S ],
    ) : RecursiveSchemaProvider.Aux[ T, Tail, S ] = new RecursiveSchemaProvider[ T, Tail ] {
        override type Shape = S

        override def provide : Schema.Aux[ T, S ] = extr.extract( inst )
    }
}

object RecursiveSchemaProvider extends RecursiveSchemaProvidersPriority1 {
    type Aux[ T, Tail <: Tuple, S ] = RecursiveSchemaProvider[ T, Tail ] { type Shape = S }

    given schemaInstanceProvider[ T, Tail <: Tuple ](
        using inst : Schema[ T ],
    ) : RecursiveSchemaProvider.Aux[ T, Tail, inst.Shape ] = new RecursiveSchemaProvider[ T, Tail ] {
        override type Shape = inst.Shape
        override def provide : Schema.Aux[ T, inst.Shape ] = inst
    }
}
