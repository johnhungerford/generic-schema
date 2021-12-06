package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionCase, FieldName}
import org.hungerford.generic.schema.types.Deriver
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{NoSchema, Schema, SchemaProvider}
import scala.compiletime.constValue

import scala.deriving.Mirror
import org.hungerford.generic.schema.product.field.UniqueFieldNames
import org.hungerford.generic.schema.product.constructor.ProductConstructor
import org.hungerford.generic.schema.product.constructor.ProductDeconstructor


trait ProductDeriver[ T ] extends Deriver[ T ] {
    type Out

    def derive : Out
}

object ProductDeriver {
    type Aux[ T, Out0 ] = ProductDeriver[ T ] { type Out = Out0 }

    type DerivedPShape[ T, R <: Tuple, RV <: Tuple ] =
        ProductShape[ T, R, RV, Nothing, Unit, RV => T, T => RV ]

    def apply[ T ](
        using
        prd : ProductDeriver[ T ],
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
        uniq : UniqueFieldNames[ Rt ],
        cType : ProductConstructor[ RVt => T, RVt, Nothing, T ],
        dcType : ProductDeconstructor[ T => RVt, RVt, Nothing, T ],
    ) : ProductDeriver[ T ] with {
            override type Out = DerivedPShape[ T, Rt, RVt ]

            override def derive : DerivedPShape[ T, Rt, RVt ] = {
                ProductShape[ T, Rt, RVt, Nothing, Unit, RVt => T, T => RVt ](
                    fieldDescriptions = fieldDeriver.derive,
                    additionalFieldsSchema = NoSchema,
                    rv => mirror.fromProduct( rv ),
                    ( value : T ) => Tuple.fromProductTyped[ T ]( value )( using mirror ),
                )
            }
    }
}

trait ProductBuildDeriver[ T ] {
    type Builder

    def derive : Builder
}

object ProductBuildDeriver {
    type Aux[ T, B ] = ProductBuildDeriver[ T ] { type Builder = B }

    type DerivedBuilder[ T, R <: Tuple, RV <: Tuple ] =
        ProductSchemaBuilder[ T, R, RV, Nothing, Unit, RV => T, T => RV ]

    def apply[ T ](
        using
        prd : ProductBuildDeriver[ T ],
    ) : ProductBuildDeriver.Aux[ T, prd.Builder ] = prd

    type MirrorProduct[ T, Elems <: Tuple, ElemLabels <: Tuple ] = Mirror.ProductOf[ T ] {
        type MirroredElemTypes = Elems
        type MirroredElemLabels = ElemLabels
    }

    inline given [ T <: Product, L <: Tuple, RVt <: Tuple, LRV <: Tuple, Rt <: Tuple ](
        using
        mirror : MirrorProduct[ T, RVt, L ],
        zip : Zipper.Aux[ L, RVt, LRV ],
        fieldDeriver : FieldDeriver.Aux[ LRV, Rt ],
//        lengther : TupleIntLength[ Rt ],
        valEv : CtxWrapTuplesConstraint[ FieldDescription, Rt, RVt ],
//        uniq : UniqueFieldNames[ Rt ],
//        cType : ProductConstructor[ RVt => T, RVt, Nothing, T ],
//        dcType : ProductDeconstructor[ T => RVt, RVt, Nothing, T ],
    ) : ProductBuildDeriver[ T ] with {
        override type Builder = DerivedBuilder[ T, Rt, RVt ]

        override def derive : DerivedBuilder[ T, Rt, RVt ] = {
            ProductSchemaBuilder[ T, Rt, RVt, Nothing, Unit, RVt => T, T => RVt ](
                None,
                Set.empty[ Validator[ T ] ],
                NoSchema,
                fieldDeriver.derive,
                rv => mirror.fromProduct( rv ),
                ( value : T ) => Tuple.fromProductTyped[ T ]( value )( using mirror ),
            )
        }
    }
}

trait FieldDeriver[ T ] extends Deriver[ T ]

object FieldDeriver {
    type Aux[ T, FS ] = FieldDeriver[ T ] { type Out = FS }

    inline given fd[ T, N <: FieldName, S ](
        using
        provider : SchemaProvider.Aux[ T, S ],
    ) : FieldDeriver.Aux[ (N, T), FieldDescription.Aux[ T, N, S ] ] = new FieldDeriver[ (N, T) ] {
            override type Out = FieldDescription.Aux[ T, N, S ]

            override def derive : Out = {
                val fn = constValue[ N ]
                FieldDescriptionCase[ T, N, S ]( fn, provider.provide )
            }
    }

    inline given hnilFDFieldDeriver : FieldDeriver[ EmptyTuple ] with {
            type Out = EmptyTuple

            override def derive : EmptyTuple = EmptyTuple
        }

    inline given hlistFDFieldDeriver[ N <: FieldName, T, S, TTail <: Tuple, Res <: Tuple ](
        using
        fdFieldDeriver : FieldDeriver.Aux[ (N, T), FieldDescription.Aux[ T, N, S ] ],
        next : FieldDeriver.Aux[ TTail, Res ],
    ) : FieldDeriver.Aux[ (N, T) *: TTail, FieldDescription.Aux[ T, N, S ] *: Res ] = {
        new FieldDeriver[ (N, T) *: TTail ] {
            override type Out = FieldDescription.Aux[ T, N, S ] *: Res

            override def derive : FieldDescription.Aux[ T, N, S ] *: Res = {
                val headRes : FieldDescription.Aux[ T, N, S ] = fdFieldDeriver.derive
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
