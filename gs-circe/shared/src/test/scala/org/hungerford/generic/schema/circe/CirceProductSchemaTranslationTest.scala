package org.hungerford.generic.schema.circe

import org.scalatest.flatspec.AnyFlatSpecLike
import org.hungerford.generic.schema.translation.{ProductJsonTranslationTest, SchemaTranslator}
import io.circe.{Decoder as CirceDecoder, Encoder as CirceEncoder, *, given}
import io.circe.syntax.*
import org.hungerford.generic.schema.product.ProductShape
import org.hungerford.generic.schema.product.translation.{Decoder, Encoder}

import scala.util.NotGiven
import org.hungerford.generic.schema.translation.ProductTranslationTestSchemata.*
import org.hungerford.generic.schema.translation.{RecursiveCoproduct, RecursiveProduct, Term}
import CirceSchemaTranslation.given
import org.hungerford.generic.schema.coproduct.subtype.LazySubtype

class CirceProductSchemaTranslationTest
  extends ProductJsonTranslationTest[ Codec ] {

    import recursiveSchemaDerived.givenSchema

    def osNoAf: Codec[ NoAF ] = SchemaTranslator.translate( noAfSchema )
    def osHasAf: Codec[ HasAF ] = SchemaTranslator.translate( hasAfSchema )
    def osHasAfPrim: Codec[ HasAF ] = SchemaTranslator.translate( hasAfPrimitiveSch )
    def osOutside: Codec[ Outside ] = SchemaTranslator.translate( outsideSch )
    def osOutsideIns: Codec[ Outside ] = SchemaTranslator.translate( outsideSchUsingInside )
    def osOutsideDer: Codec[ Outside ] = SchemaTranslator.translate( outsideSchemaDerived )
    def osRecursiveSchemaDer: Codec[ RecursiveProduct ] = SchemaTranslator.translate( recursiveSchemaDerived )
    def osNested: Codec[ NestedProduct1 ] = SchemaTranslator.translate( nestedProductSch )

    def writeJson[ T ]( value : T, schm : Codec[ T ] ) : String = {
        schm( value ).noSpaces.toString
    }

    def readJson[ T ]( value : String, schm : Codec[ T ] ) : T = {
        given Codec[ T ] = schm
        io.circe.parser.decode[ T ]( value ).toTry.get
    }

}

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
//        import generic.schema.exports.{*, given}
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

