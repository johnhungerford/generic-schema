package org.hungerford.generic.schema.coproduct

import org.hungerford.generic.schema.{ComplexSchema, RecursiveSchemaProvider, Schema, SchemaProvider}
import org.hungerford.generic.schema.types.{CtxWrapTuplesConstraint, Deriver, Sub, Zipper}
import org.hungerford.generic.schema.coproduct.subtype.{Subtype, SubtypeCase, ToSuperGenerator, TypeName}
import org.hungerford.generic.schema.product.ProductDeriver.MirrorProduct
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.field.FieldName

import scala.util.NotGiven
import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

trait CoproductDeriver[ T, TsTail <: Tuple ] {
    type Out

    def derive : Out
}

object CoproductDeriver {
    type Aux[ T, TsTail <: Tuple, O ] = CoproductDeriver[ T, TsTail ] { type Out = O }

    def apply[ T ](
        using
        cd : CoproductDeriver[ T, EmptyTuple ],
    ) : CoproductDeriver.Aux[ T, EmptyTuple, cd.Out ] = cd

    transparent inline given [ T, TsTail <: Tuple, Elems <: NonEmptyTuple, ElemLabels <: NonEmptyTuple ](
        using
        mir : Mirror.Of[ T ],
        der : CoproductMirDeriver[ T, TsTail, mir.MirroredElemTypes, mir.MirroredElemLabels ],
        ev1 : mir.MirroredElemTypes =:= Elems,
        ev2 : mir.MirroredElemLabels =:= ElemLabels,
    ) : CoproductDeriver.Aux[ T, TsTail, der.Out ] = {

        new CoproductDeriver[ T, TsTail ] {
            override type Out = der.Out
            override def derive: der.Out = der.derive
        }
    }
}

trait CoproductMirDeriver[ T, TsTail <: Tuple, Elems <: Tuple, ElemLabels <: Tuple ] {
    type Out

    def derive : Out
}

object CoproductMirDeriver {
    type Aux[ T, TsTail <: Tuple, Elems <: Tuple, ElemLabels <: Tuple, O ] =
        CoproductMirDeriver[ T, TsTail, Elems, ElemLabels ] { type Out = O }

    given [ T, TsTail <: Tuple, Elems <: Tuple, ElemLabels <: Tuple, R <: Tuple ](
        using
        der : SubtypesDeriver.Aux[ T, TsTail, Elems, ElemLabels, R ],
        ctx : CtxWrapTuplesConstraint[ Subtype.Ctx[ T, Unit ], R, Elems ],
        uniq : UniqueTypeNames[ R ],
        uniqDv : UniqueDiscriminatorValues[ R ],
        vd : ValidDiscriminator[ Unit, Nothing, R ],
    ) : CoproductMirDeriver.Aux[ T, TsTail, Elems, ElemLabels, CoproductShape[ T, R, Elems, Unit, Nothing ] ] = {
        new CoproductMirDeriver[ T, TsTail, Elems, ElemLabels ] {
            type Out = CoproductShape[ T, R, Elems, Unit, Nothing ]

            def derive: Out = CoproductShape[ T, R, Elems, Unit, Nothing ](
                der.derive( 0 )
            )
        }
    }

    def apply[ T, Elems <: Tuple, ElemLabels <: Tuple, Res ](
        mir : Mirror.SumOf[ T ] { type MirroredElemTypes = Elems; type MirroredElemLabels = ElemLabels },
    )(
        using
        cd : CoproductMirDeriver.Aux[ T, EmptyTuple, Elems, ElemLabels, Res ],
    ) : CoproductMirDeriver.Aux[ T, EmptyTuple, Elems, ElemLabels, Res ] = cd
}

trait SubtypesDeriver[ T, TsTail <: Tuple, STs <: Tuple, Ns <: Tuple ] {
    type Out <: Tuple

    def derive( ordinal : Int ) : Out
}

object SubtypesDeriver {
    type Aux[ T, TsTails <: Tuple, STs <: Tuple, Ns <: Tuple, O ] =
        SubtypesDeriver[ T, TsTails, STs, Ns ] { type Out = O }

    given [ T, TsTail <: Tuple ] : SubtypesDeriver[ T, TsTail, EmptyTuple, EmptyTuple ] with {
        type Out = EmptyTuple

        def derive( ordinal : Int ) : EmptyTuple = EmptyTuple
    }

    given [ T, TsTail <: Tuple, ST, STS, STTail <: Tuple, N <: TypeName, NTail <: Tuple, Next <: Tuple ](
        using
        mir : Mirror.SumOf[ T ],
        provider : RecursiveSchemaProvider.Aux[ ST, T *: TsTail, STS ],
        ev : NotGiven[ N =:= Nothing ],
        tsGen : ToSuperGenerator.Aux[ T, ST, ST => T ],
        tDer : SubtypesDeriver.Aux[ T, TsTail, STTail, NTail, Next ],
        nVal : ValueOf[ N ],
    ) : SubtypesDeriver.Aux[ T, TsTail, ST *: STTail, N *: NTail, Subtype.Aux[ T, ST, Unit, Nothing, Unit, N, STS ] *: Next ] = {
//        val typeName = summonInline[ ValueOf[ N ] ].value
        val typeName = nVal.value

        new SubtypesDeriver[ T, TsTail, ST *: STTail, N *: NTail ] {
            type Out = Subtype.Aux[ T, ST, Unit, Nothing, Unit, N, STS ] *: Next

            def derive( ordinal : Int ) : Out = {
                SubtypeCase[ T, ST, Unit, Nothing, Unit, N, STS ](
                    typeName,
                    provider.provide,
                    tsGen.toSuper,
                    ( t : T ) => if ( mir.ordinal( t ) == ordinal ) Some( t.asInstanceOf[ ST ] ) else None,
                    (),
                ) *: tDer.derive( ordinal + 1 )
            }
        }
    }
}
