package org.hungerford.generic.schema.bridge

import org.hungerford.generic.schema.product.ProductSchema
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionCase, TranslatedFieldDescription}
import org.hungerford.generic.schema.types.{Injector, SimpleExtractor}
import org.hungerford.generic.schema.{NoSchema, Primitive}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import shapeless._
import ujson.Value
import upickle.default
import upickle.default._

class BiMapProductSchemaBridgeTest extends AnyFlatSpecLike with Matchers {

    case class NoAF( intField : Int, strField : String )

    type RVt = Int :: String :: HNil
    type Rt = FieldDescription.Aux[ Int, Primitive[ Int ] ] :: FieldDescription.Aux[ String, Primitive[ String ] ] :: HNil
    type FDL = TranslatedFieldDescription[ Int, ReadWriter ] :: TranslatedFieldDescription[ String, ReadWriter ] :: HNil

    behavior of "BiMapProductSchemaBridge.Implicits.productTranslationWithoutAF"

    val intFieldDesc : FieldDescription.Aux[ Int, Primitive[ Int ] ] = FieldDescriptionCase[ Int, Primitive[ Int ] ]( "int_field", Primitive[ Int ]() )
    val strFieldDesc : FieldDescription.Aux[ String, Primitive[ String ] ] = FieldDescriptionCase[ String, Primitive[ String ] ]( "str_field", Primitive[ String ]() )

    it should "translate a product schema without additional fields" in {

        val generic = Generic.materialize[ NoAF, RVt ]

        implicit val testSchema = ProductSchema[ NoAF, FieldDescription.Aux[ Int, Primitive[ Int ] ] :: FieldDescription.Aux[ String, Primitive[ String ] ] :: HNil, Int :: String :: HNil, Nothing, NoSchema.type ](
            Some( "test schema description" ),
            Set.empty,
            intFieldDesc :: strFieldDesc :: HNil,
            NoSchema,
            ( rvt, _ ) => generic.from( rvt ),
            ( value ) => (generic.to( value ), Map.empty),
        )

        import org.hungerford.generic.schema.upickle.UPickleSchemaTranslation._

        implicit val noAfRw: ReadWriter[ NoAF ] = rw

        write( NoAF( 1, "hello" ) ) shouldBe """{"int_field":1,"str_field":"hello"}"""
    }

}
