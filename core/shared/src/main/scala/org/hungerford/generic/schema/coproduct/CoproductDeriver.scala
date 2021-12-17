package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.{ComplexSchema, SchemaProvider}
import org.hungerford.generic.schema.coproduct.ProductDeriver.MirrorCoproduct
import org.hungerford.generic.schema.types.{Deriver, Sub}
import org.hungerford.generic.schema.coproduct.subtype.{AsSuperGenerator, Subtype, SubtypeCase, TypeName}

import scala.deriving.Mirror
import scala.compiletime.constValue

trait ProductDeriver[ T ] extends Deriver[ T ]

object ProductDeriver {
    type Aux[ T, Out0 ] = ProductDeriver[ T ] {type Out = Out0}

    type DerivedCoproduct[ T, R <: Tuple, RV <: Tuple ] = CoproductShape[ T, R, RV, Unit, Nothing ]

    type MirrorCoproduct[ T, Elems <: Tuple, ElemLabels <: Tuple ] = Mirror.SumOf[ T ] {
        type MirroredElemTypes = Elems
        type MirroredElemLabels = ElemLabels
    }
}

trait SubtypeDeriver[ T ] extends Deriver[ T ]

object SubtypeDeriver {
    type Aux[ T, O ] = SubtypeDeriver[ T ] { type Out = O }

    inline given [ T, ST, STSub, N <: TypeName ](
        using
        provider : SchemaProvider[ ST ],
        asGen : AsSuperGenerator.Aux[ T, ST, ST => T ],
    ) : SubtypeDeriver.Aux[ (T, ST, N), Subtype.Aux[ T, ST, Unit, Nothing, Unit, N, provider.Shape ] ] = new SubtypeDeriver[ (T, ST, N) ] {
        type Out = Subtype.Aux[ T, ST, Unit, Nothing, Unit, N, provider.Shape ]

        private val typeName = constValue[ N ]

        override def derive: Out = SubtypeCase[ T, ST, Unit, Nothing, Unit, N, provider.Shape ](
            typeName,
            provider.provide,
            asGen.as,
            (),
        )
    }
}
