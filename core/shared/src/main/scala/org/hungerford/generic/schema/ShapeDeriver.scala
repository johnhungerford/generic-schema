//package org.hungerford.generic.schema
//
//import org.hungerford.generic.schema.product.{ProductDeriver, ProductShape}
//import org.hungerford.generic.schema.types.Deriver
//
//trait ShapeDeriver[ T ] extends Deriver[ T ]
//
//object ShapeDeriver {
//    type Aux[ T, Shape ] = ShapeDeriver[ T ] { type Out = Shape }
//
//    def apply[ T ](
//        implicit shd : ShapeDeriver[ T ],
//    ) : ShapeDeriver.Aux[ T, shd.Out ] = shd
//
//    implicit def productShapeDeriver[ T ](
//        implicit psd : ProductDeriver[ T ],
//    ) : ShapeDeriver.Aux[ T, psd.Out ] = new ShapeDeriver[ T ] {
//        type Out = psd.Out
//
//        override def derive : Out = psd.derive
//    }
//}
