package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.empty._
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionCase}
import org.hungerford.generic.schema.types.Deriver
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{NoSchema, Schema, SchemaProvider, product}
import scala.compiletime.constValue

import scala.deriving.Mirror

trait ProductDeriver[ T ] extends Deriver[ T ] {
    type Out

    def derive : Out
}

object ProductDeriver {
    type Aux[ T, Out0 ] = ProductDeriver[ T ] { type Out = Out0 }

    def apply[ T ](
        implicit prd : ProductDeriver[ T ],
    ) : ProductDeriver.Aux[ T, prd.Out ] = prd

    type MirrorProduct[ T, Elems <: Tuple, ElemLabels <: Tuple ] = Mirror.ProductOf[ T ] {
        type MirroredElemTypes = Elems
        type MirroredElemLabels = ElemLabels
    }

    inline given [ T <: Product, L <: Tuple, RVt <: Tuple, LRV <: Tuple, Rt <: Tuple ](
        using
        mirror : MirrorProduct[ T, RVt, L ],
        zip : Zipper.Aux[ L, RVt, LRV ],
        fieldDeriver : FieldDeriver.Aux[ LRV, Rt ],
        lengther : TupleIntLength[ Rt ],
        valEv : CtxWrapTuplesConstraint[ FieldDescription, Rt, RVt ],
    ) : ProductDeriver[ T ] with {
            override type Out = ProductShape[ T, Rt, RVt, Nothing, Unit ]

            override def derive : ProductShape[ T, Rt, RVt, Nothing, Unit ] = {
                ProductShape[ T, Rt, RVt, Nothing, Unit ](
                    fieldDescriptions = fieldDeriver.derive,
                    additionalFieldsSchema = NoSchema,
                    (rv, _) => mirror.fromProduct( rv ),
                    ( value : T ) => (Tuple.fromProductTyped[ T ]( value )( using mirror ), Map.empty),
                )
            }
    }
}

trait FieldDeriver[ T ] extends Deriver[ T ]

object FieldDeriver {
    type Aux[ T, FS ] = FieldDeriver[ T ] { type Out = FS }

    inline given fd[ T, N <: String ](
        using
        provider : SchemaProvider[ T ],
    ) : FieldDeriver.Aux[ (N, T), FieldDescription.AuxN[ T, N ] ] = new FieldDeriver[ (N, T) ] {
            override type Out = FieldDescription.AuxN[ T, N ]

            override def derive : Out = {
                val fn = constValue[ N ]
                FieldDescriptionCase[ T, N, provider.Shape ]( fn, provider.provide )
            }
    }

    inline given hnilFDFieldDeriver : FieldDeriver[ EmptyTuple ] with {
            type Out = EmptyTuple

            override def derive : EmptyTuple = EmptyTuple
        }

    inline given hlistFDFieldDeriver[ N <: String, T, TTail <: Tuple, Res <: Tuple ](
        using
        fdFieldDeriver : FieldDeriver.Aux[ (N, T), FieldDescription.AuxN[ T, N ] ],
        next : FieldDeriver.Aux[ TTail, Res ],
    ) : FieldDeriver.Aux[ (N, T) *: TTail, FieldDescription.AuxN[ T, N ] *: Res ] = {
        new FieldDeriver[ (N, T) *: TTail ] {
            override type Out = FieldDescription.AuxN[ T, N ] *: Res

            override def derive : FieldDescription.AuxN[ T, N ] *: Res = {
                val headRes : FieldDescription.AuxN[ T, N ] = fdFieldDeriver.derive
                headRes *: next.derive
            }
        }
    }
}

sealed trait Zipper[ A, B ] {
    type Out
}

trait LowPriorityZippers {
    inline given [ A, B ] : Zipper[ A, B ] with {
        type Out = (A, B)
    }
}

object Zipper {
    type Aux[ A, B, Res ] = Zipper[ A, B ] { type Out = Res }

    inline given Zipper[ EmptyTuple, EmptyTuple ] with {
        type Out = EmptyTuple
    }

    inline given [ HeadL, TailL <: Tuple, HeadR, TailR <: Tuple, Res <: Tuple ](
        using zip : Zipper.Aux[ TailL, TailR, Res ],
    ) : Zipper[ HeadL *: TailL, HeadR *: TailR ] with {
        type Out = (HeadL, HeadR) *: zip.Out
    }
}
