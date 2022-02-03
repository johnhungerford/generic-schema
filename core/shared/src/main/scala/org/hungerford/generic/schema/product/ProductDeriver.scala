package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{Field, LazyField, FieldName}
import org.hungerford.generic.schema.types.{Contains, CtxWrapTuplesConstraint, Deriver, Zipper}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{NoSchema, Schema, RecursiveSchemaProvider}

import scala.compiletime.constValue
import scala.deriving.Mirror
import scala.util.NotGiven
import org.hungerford.generic.schema.product.field.UniqueFieldNames
import org.hungerford.generic.schema.product.constructor.ProductConstructor
import org.hungerford.generic.schema.product.constructor.ProductDeconstructor


trait ProductDeriver[ T, Tail <: Tuple ] {
    type Out

    def derive : Out
}

object ProductDeriver {
    type Aux[ T, Tail <: Tuple, Out0 ] = ProductDeriver[ T, Tail ] { type Out = Out0 }

    type DerivedPShape[ T, R <: Tuple, RV <: Tuple ] =
        ProductShape[ T, R, RV, Nothing, Unit, Unit, RV => T ]

    def apply[ T ](
        using
        prd : ProductDeriver[ T, EmptyTuple ],
    ) : ProductDeriver.Aux[ T, EmptyTuple, prd.Out ] = prd

    type MirrorProduct[ T, Elems <: Tuple, ElemLabels <: Tuple ] = Mirror.ProductOf[ T ] {
        type MirroredElemTypes = Elems
        type MirroredElemLabels = ElemLabels
    }

    inline given [ T <: Product, L <: Tuple, RVt <: Tuple, LRV <: Tuple, Rt <: Tuple, TsTail <: Tuple ](
        using
        mirror : MirrorProduct[ T, RVt, L ],
        mirEv : NotGiven[ mirror.type <:< Mirror.Singleton ],
        zip : Zipper.Aux[ L, RVt, LRV ],
        fieldDeriver : FieldDeriver.Aux[ (T, TsTail, LRV), T => RVt, Rt ],
        valEv : CtxWrapTuplesConstraint[ Field.Of, Rt, RVt ],
        uniq : UniqueFieldNames[ Rt ],
        cType : ProductConstructor[ RVt => T, RVt, Nothing, T ],
    ) : ProductDeriver[ T, TsTail ] with {
            override type Out = DerivedPShape[ T, Rt, RVt ]

            override def derive : DerivedPShape[ T, Rt, RVt ] = {
                val extractor : T => RVt = ( t : T ) =>
                    Tuple.fromProduct( t ).asInstanceOf[ RVt ]

                ProductShape[ T, Rt, RVt, Nothing, Unit, Unit, RVt => T ](
                    fieldDescriptions = fieldDeriver.derive( extractor ),
                    additionalFieldsSchema = NoSchema,
                    afExtractor = (),
                    constructor = rv => mirror.fromProduct( rv ),
                )
            }
    }
}

//trait ProductBuildDeriver[ T ] {
//    type Builder
//
//    def derive : Builder
//}
//
//object ProductBuildDeriver {
//    type Aux[ T, B ] = ProductBuildDeriver[ T ] { type Builder = B }
//
//    type DerivedBuilder[ T, R <: Tuple, RV <: Tuple ] =
//        ProductSchemaBuilder[ T, R, RV, Nothing, Unit, RV => T, T => RV ]
//
//    def apply[ T ](
//        using
//        prd : ProductBuildDeriver[ T ],
//    ) : ProductBuildDeriver.Aux[ T, prd.Builder ] = prd
//
//    type MirrorProduct[ T, Elems <: Tuple, ElemLabels <: Tuple ] = Mirror.ProductOf[ T ] {
//        type MirroredElemTypes = Elems
//        type MirroredElemLabels = ElemLabels
//    }
//
//    inline given [ T <: Product, L <: Tuple, RVt <: Tuple, LRV <: Tuple, Rt <: Tuple ](
//        using
//        mirror : MirrorProduct[ T, RVt, L ],
//        zip : Zipper.Aux[ L, RVt, LRV ],
//        fieldDeriver : FieldDeriver.Aux[ LRV, Rt ],
////        lengther : TupleIntLength[ Rt ],
//        valEv : CtxWrapTuplesConstraint[ Field, Rt, RVt ],
////        uniq : UniqueFieldNames[ Rt ],
////        cType : ProductConstructor[ RVt => T, RVt, Nothing, T ],
////        dcType : ProductDeconstructor[ T => RVt, RVt, Nothing, T ],
//    ) : ProductBuildDeriver[ T ] with {
//        override type Builder = DerivedBuilder[ T, Rt, RVt ]
//
//        override def derive : DerivedBuilder[ T, Rt, RVt ] = {
//            ProductSchemaBuilder[ T, Rt, RVt, Nothing, Unit, RVt => T, T => RVt ](
//                None,
//                None,
//                Set.empty[ Validator[ T ] ],
//                Nil,
//                false,
//                NoSchema,
//                fieldDeriver.derive,
//                rv => mirror.fromProduct( rv ),
//                ( value : T ) => Tuple.fromProductTyped[ T ]( value )( using mirror ),
//            )
//        }
//    }
//}

trait FieldDeriver[ T, Extr ] {
    type Out

    def derive( extractor : Extr ) : Out
}

object FieldDeriver {
    type Aux[ T, Extr, FS ] = FieldDeriver[ T, Extr ] { type Out = FS }

    inline given fd[ T, TsTail <: Tuple, F, N <: FieldName, S ](
        using
        ev : NotGiven[ Contains[ T *: TsTail, F ] ],
        provider : RecursiveSchemaProvider.Aux[ F, T *: TsTail, S ],
    ) : FieldDeriver.Aux[ (T, TsTail, N, F), T => F, Field[ T, F, N, S ] ] = new FieldDeriver[ (T, TsTail, N, F), T => F ] {
            override type Out = Field[ T, F, N, S ]

            override def derive( extr : T => F ) : Out = {
                val fn = constValue[ N ]
                Field[ T, F, N, S ]( fn, extr, provider.provide )
            }
    }

    inline given fdLazy[ T, TsTail <: Tuple, F, N <: FieldName ](
        using
        ev : Contains[ T *: TsTail, F ],
    ) : FieldDeriver.Aux[ (T, TsTail, N, F), T => F, LazyField[ T, F, N ] ] = new FieldDeriver[ (T, TsTail, N, F), T => F ] {
        override type Out = LazyField[ T, F, N ]

        override def derive( extr : T => F ) : Out = {
            val fn = constValue[ N ]
            LazyField[ T, F, N ]( fn, extr )
        }
    }

    inline given hnilFDFieldDeriver[ T, TsTail <: Tuple, Extr ] : FieldDeriver[ (T, TsTail, EmptyTuple), Extr ] with {
            type Out = EmptyTuple

            override def derive( extractor : Extr ) : EmptyTuple = EmptyTuple
        }

    inline given hlistFDFieldDeriver[ N <: FieldName, T, TsTail <: Tuple, F, Fld <: Field.OrLazy[ T, F, N ], VTail <: Tuple, TTail <: Tuple, Res <: Tuple ](
        using
        fdFieldDeriver : FieldDeriver.Aux[ (T, TsTail, N, F), T => F, Fld ],
        next : FieldDeriver.Aux[ (T, TsTail, TTail), T => VTail, Res ],
    ) : FieldDeriver.Aux[ (T, TsTail, (N, F) *: TTail), T => F *: VTail, Fld *: Res ] = {
        new FieldDeriver[ (T, TsTail, (N, F) *: TTail), T => F *: VTail ] {
            override type Out = Fld *: Res

            override def derive( extractor : T => F *: VTail ) : Fld *: Res = {
                val headExtr : T => F = ( t : T ) => extractor( t ).head
                val headRes : Fld = fdFieldDeriver.derive( headExtr )
                val tailExtr : T => VTail = ( t : T ) => extractor( t ).tail
                headRes *: next.derive( tailExtr )
            }
        }
    }
}
