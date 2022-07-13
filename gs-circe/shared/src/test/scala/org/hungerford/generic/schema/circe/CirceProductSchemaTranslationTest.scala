package org.hungerford.generic.schema.circe

//import org.scalatest.flatspec.AnyFlatSpecLike
//import org.hungerford.generic.schema.translation.{ProductJsonTranslationTest, SchemaTranslator}
//import io.circe.{Decoder as CirceDecoder, Encoder as CirceEncoder, *, given}
//import io.circe.syntax.*
//import org.hungerford.generic.schema.product.ProductShape
//import org.hungerford.generic.schema.product.translation.{Decoder, Encoder}
//
//import scala.util.NotGiven
//
//import CirceSchemaTranslation.given
//
//class CirceProductSchemaTranslationTest
//  extends ProductJsonTranslationTest[ Codec ] {
//
//    def writeJson[ T ]( value : T, schm : Codec[ T ] ) : String = {
//        schm( value ).noSpaces.toString
//    }
//
//}

//import org.scalatest.flatspec.AnyFlatSpecLike
//import org.hungerford.generic.schema.translation.{ProductJsonTranslationTest, ProductTranslationTestSchemata, RecursiveSchemaTranslator, SchemaTranslator}
//import io.circe.{Decoder as CirceDecoder, Encoder as CirceEncoder, *, given}
//import io.circe.syntax.*
//import org.hungerford.generic.schema.product.ProductShape
//import org.hungerford.generic.schema.product.translation.{Decoder, Encoder}
//
//import scala.util.NotGiven
//import CirceSchemaTranslation.given
//import org.hungerford.generic.schema.Schema
//import org.hungerford.generic.schema.translation.ProductTranslationTestSchemata.{RecursiveProduct, recursiveSchemaDerived}

//object Schemata {
//    case class RecProd(field: RecProd)
//
//    val schema = {
//        import org.hungerford.generic.schema.Default.dsl.{*, given}
//
//        Schema.derived[ RecProd ]
//    }
//}
//
//class CirceProductSchemaTranslationTest extends AnyFlatSpecLike {
//
////    val recProdShape = recursiveSchemaDerived.shape
////
////    CirceSchemaTranslation.writeFields[ RecursiveProduct, recProdShape.R, RecursiveSchemaTranslator[ RecursiveProduct, recursiveSchemaDerived.Shape, EmptyTuple, CirceEncoder ] *: EmptyTuple ]
////
////    SchemaTranslator.translate[ RecursiveProduct, recursiveSchemaDerived.Shape, CirceEncoder ](recursiveSchemaDerived)
//
////    import Schemata.*
////
////    import schema.givenSchema
////
////    val sch = summon[ Schema[ RecProd ] ]
////
////    val schemaShape = sch.shape
////
////    SchemaTranslator.translate[ RecProd, schema.Shape, Codec ]( schema )
//}

