package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.{ComplexSchema, Schema, SchemaProvider}
import org.hungerford.generic.schema.types.{CtxWrapTuplesConstraint, Deriver, Sub, Zipper}
import org.hungerford.generic.schema.coproduct.subtype.{FromSuperGenerator, Subtype, SubtypeCase, ToSuperGenerator, TypeName}
import org.hungerford.generic.schema.product.ProductDeriver.MirrorProduct
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.FieldName

import scala.util.NotGiven
import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

trait CoproductDeriver[ T ] {
    type Out

    def derive : Out
}

object CoproductDeriver {
    type Aux[ T, O ] = CoproductDeriver[ T ] { type Out = O }

    def apply[ T ](
        using
        cd : CoproductDeriver[ T ],
    ) : CoproductDeriver.Aux[ T, cd.Out ] = cd

    transparent inline given [ T, Elems <: NonEmptyTuple, ElemLabels <: NonEmptyTuple ](
        using
        mir : Mirror.Of[ T ],
        der : CoproductMirDeriver[ T, mir.MirroredElemTypes, mir.MirroredElemLabels ],
        ev1 : mir.MirroredElemTypes =:= Elems,
        ev2 : mir.MirroredElemLabels =:= ElemLabels,
    ) : CoproductDeriver.Aux[ T, der.Out ] = {

        new CoproductDeriver[ T ] {
            override type Out = der.Out
            override def derive: der.Out = der.derive
        }
    }
}

trait CoproductMirDeriver[ T, Elems <: Tuple, ElemLabels <: Tuple ] {
    type Out

    def derive : Out
}

object CoproductMirDeriver {
    type Aux[ T, Elems <: Tuple, ElemLabels <: Tuple, O ] = CoproductMirDeriver[ T, Elems, ElemLabels ] { type Out = O }

    given [ T, Elems <: Tuple, ElemLabels <: Tuple, R <: Tuple ](
        using
        der : SubtypesDeriver.Aux[ T, Elems, ElemLabels, R ],
        ctx : CtxWrapTuplesConstraint[ Subtype.Ctx[ T, Unit ], R, Elems ],
        uniq : UniqueTypeNames[ R ],
        uniqDv : UniqueDiscriminatorValues[ R ],
        vd : ValidDiscriminator[ Unit, Nothing, R ],
    ) : CoproductMirDeriver.Aux[ T, Elems, ElemLabels, CoproductShape[ T, R, Elems, Unit, Nothing ] ] = {
        new CoproductMirDeriver[ T, Elems, ElemLabels ] {
            type Out = CoproductShape[ T, R, Elems, Unit, Nothing ]

            def derive: Out = CoproductShape[ T, R, Elems, Unit, Nothing ](
                der.derive
            )
        }
    }

    def apply[ T, Elems <: Tuple, ElemLabels <: Tuple, Res ](
        mir : Mirror.SumOf[ T ] { type MirroredElemTypes = Elems; type MirroredElemLabels = ElemLabels },
    )(
        using
        cd : CoproductMirDeriver.Aux[ T, Elems, ElemLabels, Res ],
    ) : CoproductMirDeriver.Aux[ T, Elems, ElemLabels, Res ] = cd
}

trait SubtypesDeriver[ T, STs <: Tuple, Ns <: Tuple ] {
    type Out <: Tuple

    def derive : Out
}

object SubtypesDeriver {
    type Aux[ T, STs <: Tuple, Ns <: Tuple, O ] = SubtypesDeriver[ T, STs, Ns ] { type Out = O }

    inline given [ T ] : SubtypesDeriver[ T, EmptyTuple, EmptyTuple ] with {
        type Out = EmptyTuple

        def derive : EmptyTuple = EmptyTuple
    }

    inline given [ T, ST, STS, STTail <: Tuple, N <: TypeName, NTail <: Tuple, Next <: Tuple ](
        using
        provider : SchemaProvider.Aux[ ST, STS ],
        ev : NotGiven[ N =:= Nothing ],
        tsGen : ToSuperGenerator.Aux[ T, ST, ST => T ],
        fsGen : FromSuperGenerator.Aux[ T, ST, T => Option[ ST ] ],
        tDer : SubtypesDeriver.Aux[ T, STTail, NTail, Next ],
    ) : SubtypesDeriver.Aux[ T, ST *: STTail, N *: NTail, Subtype.Aux[ T, ST, Unit, Nothing, Unit, N, STS ] *: Next ] = {
        val typeName = summonInline[ ValueOf[ N ] ].value

        new SubtypesDeriver[ T, ST *: STTail, N *: NTail ] {
            type Out = Subtype.Aux[ T, ST, Unit, Nothing, Unit, N, STS ] *: Next

            def derive : Out = {
                SubtypeCase[ T, ST, Unit, Nothing, Unit, N, STS ](
                    typeName,
                    provider.provide,
                    tsGen.toSuper,
                    fsGen.fromSuper,
                    (),
                ) *: tDer.derive
            }
        }
    }
}
