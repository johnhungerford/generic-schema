package org.hungerford.generic.schema

import org.hungerford.generic.schema.product.{ProductDeriver, ProductSchema}
import org.hungerford.generic.schema.product.field.{FieldDescription, FieldDescriptionBuilder}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import shapeless.LabelledGeneric.Aux
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.record.Keys
import shapeless.syntax.singleton.mkSingletonOps
import upickle.default._


class SchemaDeriverTest extends AnyFlatSpecLike with Matchers {

    behavior of "SchemaDeriver"

    it should "derive a product schema equivalent to one built with implicit primitives and no additional fields" in {
        case class Test( int : Int, str : String )

        import primitives._

        type Rt = FieldDescription.Aux[ Int, Primitive[ Int ] ] :: FieldDescription.Aux[ String, Primitive[ String ] ] :: HNil
        type RVt = Int :: String :: HNil
        type Tupt = (Int, String)

        val testSchema : ProductSchema[ Test, FieldDescription.Aux[Int, Primitive[Int]] :: FieldDescription.Aux[String, Primitive[String]] :: HNil, Int :: String :: HNil, Nothing, NoSchema.type, (Int, String) ] = SchemaBuilder[ Test ]
          .product
          .addField( FieldDescriptionBuilder[ Int ].fromSchema.fieldName( "int" ).build )
          .addField( FieldDescriptionBuilder[ String ].fromSchema.fieldName( "str" ).build )
          .construct( (tup, _) => Test( tup._1, tup._2) )
          .deconstruct( value => ((value.int, value.str), Map.empty) )
          .build

        val newTestRes = SchemaDeriver.schema[ Test ]

        newTestRes.fieldDescriptions shouldBe testSchema.fieldDescriptions

    }

}
