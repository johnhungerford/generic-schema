//package org.hungerford.generic.schema
//
//import org.hungerford.generic.schema.product.field.{FieldDescription, FieldNamesCollector}
//import org.hungerford.generic.schema.product.{CtxWrapTuplesConstraint, TupleIntLength, ProductDeriver, ProductShape}
//import org.hungerford.generic.schema.types.Deriver
//import org.hungerford.generic.schema.validator.Validator
//
//
//trait SchemaDeriver[ T ] {
//    type Shape
//
//    def derive : Schema.Aux[ T, Shape ]
//}
//
//object SchemaDeriver {
//    type Aux[ T, S ] = SchemaDeriver[ T ] { type Shape = S }
//
//    def apply[ T ](
//        implicit sd : SchemaDeriver[ T ],
//    ) : SchemaDeriver.Aux[ T, sd.Shape ] = sd
//
//
//    implicit def productSchemaDeriver[ T ](
//        implicit prd : ProductDeriver[ T ],
//    ) : SchemaDeriver.Aux[ T, prd.Out ] = new SchemaDeriver[ T ] {
//        override type Shape = prd.Out
//
//        override def derive : Schema.Aux[ T, Shape ] = ComplexSchema[ T, Shape ]( prd.derive )
//    }
//
//    def schema[ T ](
//        implicit schemaDeriver : SchemaDeriver[ T ],
//    ) : Schema.Aux[ T, schemaDeriver.Shape ] = schemaDeriver.derive
//
//}
