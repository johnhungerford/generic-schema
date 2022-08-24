package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{Field, FieldName, LazyField}
import org.hungerford.generic.schema.types.{Contains, CtxWrapTuplesConstraint, Deriver, Size, Zipper}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{NoSchema, Primitive, RecursiveSchemaDeriver, RecursiveSchemaProvider, Schema}

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

    given [ T <: Product, TsTail <: Tuple, L <: Tuple, RVt <: Tuple, LRV <: Tuple, Rt <: Tuple ](
		using
		mirror : MirrorProduct[ T, RVt, L ],
		mirEv : NotGiven[ mirror.type <:< Mirror.Singleton ],
		zip : Zipper.Aux[ L, RVt, LRV ],
		fieldsDeriver : FieldTupDeriver.Aux[ T, TsTail, LRV, RVt, Rt ],
		valEv : CtxWrapTuplesConstraint[ Field.Tpe, Rt, RVt ],
		uniq : UniqueFieldNames[ Rt ],
		cType : ProductConstructor[ RVt => T, RVt, Nothing, T ],
    ) : ProductDeriver[ T, TsTail ] with {
            override type Out = DerivedPShape[ T, Rt, RVt ]

            override def derive : DerivedPShape[ T, Rt, RVt ] = {
                val extractor : T => RVt = ( t : T ) =>
                    Tuple.fromProduct( t ).asInstanceOf[ RVt ]

                ProductShape[ T, Rt, RVt, Nothing, Unit, Unit, RVt => T ](
                    fieldDescriptions = fieldsDeriver.derive( extractor ),
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

trait FieldDeriver[ T, TsTail <: Tuple, F, N <: FieldName ] {
    type Out <: Field.OrLazy[ T, F, N ]

    def derive( extractor : T => F ) : Out
}


//trait FDPriority3 {
//    given fdPrimitive[ T, TsTail <: Tuple, F, N <: FieldName ](
//        using
//        nVal : ValueOf[ N ],
//    ) : FieldDeriver.Aux[ (T, TsTail, N, F), T => F, Field[ T, F, N, Unit ] ] = new FieldDeriver[ (T, TsTail, N, F), T => F ] {
//        override type Out = Field[ T, F, N, Unit ]
//
//        override def derive( extr : T => F ) : Out = {
//            val fn = nVal.value
//            Field[ T, F, N, Unit ]( fn, extr, Primitive[ F ]() )
//        }
//    }
//}
//
//trait FDPriority2 extends FDPriority3  {
//    given fdDerived[ T, TsTail <: Tuple, F, N <: FieldName, S ](
//        using
//        der : RecursiveSchemaDeriver.Aux[ F, T *: TsTail, S ],
//        nVal : ValueOf[ N ],
//    ) : FieldDeriver.Aux[ (T, TsTail, N, F), T => F, Field[ T, F, N, S ] ] = new FieldDeriver[ (T, TsTail, N, F), T => F ] {
//        override type Out = Field[ T, F, N, S ]
//
//        override def derive( extr : T => F ) : Out = {
//            val fn = nVal.value
//            Field[ T, F, N, S ]( fn, extr, der.derive )
//        }
//    }
//}

trait FDPriority2 {
    given fdSchema[ T, TsTail <: Tuple, F, N <: FieldName, S ](
        using
        pr : RecursiveSchemaProvider.Aux[ F, T *: TsTail, S ],
        nVal : ValueOf[ N ],
    ) : FieldDeriver.Aux[ T, TsTail, F, N, Field[ T, F, N, S ] ] = new FieldDeriver[ T, TsTail, F, N ] {
        override type Out = Field[ T, F, N, S ]

        override def derive( extr : T => F ) : Out = {
            val fn = nVal.value
            Field[ T, F, N, S ]( fn, extr, pr.provide )
        }
    }
}

trait FDPriority1 extends FDPriority2 {

    given fdLazy2[ T, TsTail <: Tuple, F, N <: FieldName ](
        using
        ev : Contains[ TsTail, F ],
        nVal : ValueOf[ N ],
    ) : FieldDeriver.Aux[ T, TsTail, F, N, LazyField[ T, F, N ] ] = new FieldDeriver[ T, TsTail, F, N ] {
        override type Out = LazyField[ T, F, N ]

        override def derive( extr : T => F ) : Out = {
            val fn = nVal.value
            LazyField[ T, F, N ]( fn, extr )
        }
    }

}

object FieldDeriver extends FDPriority1 {
    type Aux[ T, TsTail <: Tuple, F, N <: FieldName, Fld <: Field.OrLazy[ T, F, N ] ] = FieldDeriver[ T, TsTail, F, N ] { type Out = Fld }

    given fdLazy1[ TsTail <: Tuple, F, N <: FieldName ](
        using
        nVal : ValueOf[ N ],
    ) : FieldDeriver.Aux[ F, TsTail, F, N, LazyField[ F, F, N ] ] = new FieldDeriver[ F, TsTail, F, N ] {
        override type Out = LazyField[ F, F, N ]

        override def derive( extr : F => F ) : Out = {
            val fn = nVal.value
            LazyField[ F, F, N ]( fn, extr )
        }
    }

}

trait FieldTupDeriver[ T, TsTail <: Tuple, Fs <: Tuple, Extr ] {
    type Out <: Tuple

    def derive( extract : T => Extr ) : Out
}

object FieldTupDeriver {
    type Aux[ T, TsTail <: Tuple, Fs <: Tuple, Extr, O <: Tuple ] = FieldTupDeriver[ T, TsTail, Fs, Extr ] { type Out = O }

    given hnilFDFieldDeriver[ T, TsTail <: Tuple, Extr ] : FieldTupDeriver[ T, TsTail, EmptyTuple, Extr ] with {
        type Out = EmptyTuple

        override def derive( extractor : T => Extr ) : EmptyTuple = EmptyTuple
    }

    given hlistLazyFDFieldDeriver[ T, TsTail <: Tuple, F, N <: FieldName, VTail <: Tuple, TTail <: Tuple, Res <: Tuple ](
        using
        fd : FieldDeriver[ T, TsTail, F, N ],
        ev : Contains[ T *: TsTail, F ],
        next : FieldTupDeriver.Aux[ T, TsTail, TTail, VTail, Res ],
    ) : FieldTupDeriver.Aux[ T, TsTail, (N, F) *: TTail, F *: VTail, fd.Out *: Res ] = {
        new FieldTupDeriver[ T, TsTail, (N, F) *: TTail, F *: VTail ] {
            override type Out = fd.Out *: Res

            override def derive( extractor : T => F *: VTail ) : fd.Out *: Res = {
                val headExtr : T => F = ( t : T ) => extractor( t ).head
                val headRes : fd.Out = fd.derive( headExtr )
                val tailExtr : T => VTail = ( t : T ) => extractor( t ).tail
                headRes *: next.derive( tailExtr )
            }
        }
    }

    given hlistFDFieldDeriver[ T, TsTail <: Tuple, F, N <: FieldName, VTail <: Tuple, TTail <: Tuple, Res <: Tuple ](
        using
        fd : FieldDeriver[ T, TsTail, F, N ],
        ev : NotGiven[ Contains[ T *: TsTail, F ] ],
        next : FieldTupDeriver.Aux[ T, TsTail, TTail, VTail, Res ],
    ) : FieldTupDeriver.Aux[ T, TsTail, (N, F) *: TTail, F *: VTail, fd.Out *: Res ] = {
        new FieldTupDeriver[ T, TsTail, (N, F) *: TTail, F *: VTail ] {
            override type Out = fd.Out *: Res

            override def derive( extractor : T => F *: VTail ) : fd.Out *: Res = {
                val headExtr : T => F = ( t : T ) => extractor( t ).head
                val headRes : fd.Out = fd.derive( headExtr )
                val tailExtr : T => VTail = ( t : T ) => extractor( t ).tail
                headRes *: next.derive( tailExtr )
            }
        }
    }
}
