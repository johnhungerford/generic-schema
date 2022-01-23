package org.hungerford.generic.schema.product

import org.hungerford.generic.schema.product.field.{Field, FieldCase, FieldName}
import org.hungerford.generic.schema.types.{CtxWrapTuplesConstraint, Deriver, Zipper}
import org.hungerford.generic.schema.validator.Validator
import org.hungerford.generic.schema.{NoSchema, Schema, SchemaProvider}

import scala.compiletime.constValue
import scala.deriving.Mirror
import scala.util.NotGiven
import org.hungerford.generic.schema.product.field.UniqueFieldNames
import org.hungerford.generic.schema.product.constructor.ProductConstructor
import org.hungerford.generic.schema.product.constructor.ProductDeconstructor


trait ProductDeriver[ T ] extends Deriver[ T ]

object ProductDeriver {
    type Aux[ T, Out0 ] = ProductDeriver[ T ] { type Out = Out0 }

    type DerivedPShape[ T, R <: Tuple, RV <: Tuple ] =
        ProductShape[ T, R, RV, Nothing, Unit, Unit, RV => T ]

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
        mirEv : NotGiven[ mirror.type <:< Mirror.Singleton ],
        zip : Zipper.Aux[ L, RVt, LRV ],
        fieldDeriver : FieldDeriver.Aux[ LRV, T => RVt, Rt ],
        valEv : CtxWrapTuplesConstraint[ Field.Ctx[ T ], Rt, RVt ],
        uniq : UniqueFieldNames[ Rt ],
        cType : ProductConstructor[ RVt => T, RVt, Nothing, T ],
    ) : ProductDeriver[ T ] with {
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

    inline given fd[ T, F, N <: FieldName, S ](
        using
        provider : SchemaProvider.Aux[ F, S ],
    ) : FieldDeriver.Aux[ (T, N, F), T => F, Field.Aux[ T, F, N, S ] ] = new FieldDeriver[ (T, N, F), T => F ] {
            override type Out = Field.Aux[ T, F, N, S ]

            override def derive( extr : T => F ) : Out = {
                val fn = constValue[ N ]
                FieldCase[ T, F, N, S ]( fn, extr, provider.provide )
            }
    }

    inline given hnilFDFieldDeriver[ Extr ] : FieldDeriver[ EmptyTuple, Extr ] with {
            type Out = EmptyTuple

            override def derive( extractor : Extr ) : EmptyTuple = EmptyTuple
        }

    inline given hlistFDFieldDeriver[ N <: FieldName, T, F, S, VTail <: Tuple, TTail <: Tuple, Res <: Tuple ](
        using
        fdFieldDeriver : FieldDeriver.Aux[ (T, N, F), T => F, Field.Aux[ T, F, N, S ] ],
        next : FieldDeriver.Aux[ TTail, T => VTail, Res ],
    ) : FieldDeriver.Aux[ (T, N, F) *: TTail, T => F *: VTail, Field.Aux[ T, F, N, S ] *: Res ] = {
        new FieldDeriver[ (T, N, F) *: TTail, T => F *: VTail ] {
            override type Out = Field.Aux[ T, F, N, S ] *: Res

            override def derive( extractor : T => F *: VTail ) : Field.Aux[ T, F, N, S ] *: Res = {
                val headExtr : T => F = ( t : T ) => extractor( t ).head
                val headRes : Field.Aux[ T, F, N, S ] = fdFieldDeriver.derive( headExtr )
                val tailExtr : T => VTail = ( t : T ) => extractor( t ).tail
                headRes *: next.derive( tailExtr )
            }
        }
    }
}
